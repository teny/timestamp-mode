;;; timestamp-mode.el --- Buries timestamp per operation for logging.

;; Copyright (C) 2011 teny.

;; Author: teny <ten@cool.nifty.jp>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar timestamp-mode-enable nil
	"comint-mode のバッファに文字列を埋め込む処理を有効にするか否か。
nil 以外であれば有効、nil なら無効。")

(defvar timestamp-insert-stamp-start-mark nil
	"comint が、プロセスからの output をバッファに挿入するときの開始位置。")

(defgroup timestamp nil
  "The faces used for timestamp."
  :group 'timestamp)

(defcustom timestamp-insert-stamp-only-BOL-p t
	"不可視文字列を挿入するのを、行頭だけに絞る。
行の途中に挿入すると、検索結果がバッファの可視状況と一致しなくなることを防ぐ。
nil 以外であれば行頭にのみ挿入する、nil なら行頭以外にも挿入することがある。"
  :group 'timestamp)
(defcustom timestamp-mode-string-default " TS"
	"timestamp-mode 有効時にモードラインに表示するモード文字列。"
  :group 'timestamp)
(defcustom timestamp-mode-string-suffix "*"
	"タイムスタンプ可視状態時にモードラインに付加する文字列。"
  :group 'timestamp)
(defcustom timestamp-insert-stamp-start-separater "|"
	"挿入したタイムスタンプを囲む先頭のセパレータ。"
  :group 'timestamp)
(defcustom timestamp-insert-stamp-end-separater "|"
	"挿入したタイムスタンプを囲む末尾のセパレータ。"
  :group 'timestamp)

(defface timestamp
  '((((class color) (background light))
     (:foreground "yellow green" :background "ghost white"))
    (((class color) (background dark)) (:foreground "gray85")))
  "Face for timestamp area."
  :group 'timestamp)

(defun timestamp-insert-stamp (&optional string)
	"comint を介したバッファ (shell, ssh...) で、バッファにテキストを挿入する
ときに、タイムスタンプを挿入する。
デフォルトでは、挿入したタイムスタンプは不可視とするため、操作自体は通常と
変わらない。"

	(when timestamp-mode
		(if (not (string= string ""))
				(save-excursion
					(let ((start timestamp-insert-stamp-start-mark)
								(cont t))
						(goto-char start)
						(while cont
							(when (and (not (= (point) (point-max)))
												 ;; `timestamp-insert-stamp-only-BOL-p' が nil 以外なら、
												 ;; 行の途中に不可視文字列を挿入しない様に。
												 (or (not timestamp-insert-stamp-only-BOL-p)
														 (if (> (point) 1)
																 ;; バッファの先頭以外では、直前の文字が改行の
																 ;; ときのみ挿入。
																 (string= "\n"
																					 (buffer-substring-no-properties
																						(1- (point)) (point)))
															 ;; バッファの先頭では無条件に挿入。
															 t)))
								(timestamp-insert-stamp-insert))
							(unless (timestamp-insert-next-line)
								(setq cont nil))))
					;; 次の挿入開始位置を保存。
					(set-marker timestamp-insert-stamp-start-mark (point-marker)))

			;; 挿入開始位置の保存。
			(setq timestamp-insert-stamp-start-mark (point-marker))
			;; コマンド入力時には、入力した行のタイムスタンプを更新する。
			(save-excursion
				(forward-line -1)
				(timestamp-delete-stamp)
				(timestamp-insert-stamp-insert)))))

(defun timestamp-delete-stamp ()
	"現在位置に挿入されたタイムスタンプを削除する。"
	(let* ((ovls (overlays-at (point)))
				 (ovl (let ((invisible-ovl nil))
								(while ovls
									(let* ((ovl (car ovls)))
										(when (overlay-get ovl 'invisible)
											(set 'invisible-ovl ovl)))
									(set 'ovls (cdr ovls)))
							invisible-ovl)))
		(when (overlayp ovl)
			(delete-region (overlay-start ovl) (overlay-end ovl))
			(delete-overlay ovl))))

(defun timestamp-insert-stamp-get ()
	"タイムスタンプとする文字列として、現在時刻から生成した文字列を取得する。
現状では、フォーマットは固定になっている。"
	(let* ((date-alist '(("Jan"   1) ("Feb"  2) ("Mar"  3)
											 ("Apr"   4) ("May"  5) ("Jun"  6)
											 ("Jul"   7) ("Aug"  8) ("Sep"  9)
											 ("Oct"  10) ("Nov" 11) ("Dec" 12)))
				 (date (current-time-string))
				 (date-time-str
					(format "%02d-%02d-%02d %02d:%02d:%02d"
									(string-to-number (substring date 22 24))
									(car (cdr (assoc (substring date 4 7) date-alist)))
									(string-to-number (substring date 8 10))
									(string-to-number (substring date 11 13))
									(string-to-number (substring date 14 16))
									(string-to-number (substring date 17 19)))))
	(concat timestamp-insert-stamp-start-separater date-time-str
					timestamp-insert-stamp-end-separater)))

(defun timestamp-insert-next-line ()
	"次の行の行頭にポイントを移動する。
何故か forward-line が期待した動作 (Info に記載の動作) をしない様なので……
行移動が無かったら (最終行だったら) 0 以外を返す筈なのに 0 が返されてしまう。
最終行にあるときを識別できる様にするため、行移動があったときは t, 行移動が
無かったときは nil を返す。"
	(skip-chars-forward "^\n")
	(condition-case nil
			(not (forward-char 1))
		(error nil)))

(defun timestamp-insert-stamp-insert (&optional flag)
	"タイムスタンプとなる文字列を挿入する。
挿入した文字列には、不可視属性とフェイス (timestamp) を設定する。"
	(let ((string (timestamp-insert-stamp-get))
				(start (point)))
		(insert string)
		(let ((ovl (make-overlay start (point))))
			(overlay-put ovl 'invisible 'shell-timestamp)
			(overlay-put ovl 'face 'timestamp))))

(defun timestamp-insert-stamp-visible (&optional flag)
	"不可視属性の有効、無効をトグル操作する。
また、挿入文字列が可視になることで表示量が増えるため、truncate-lines を t に
変更して見易い状態を保つ様にする。
前置引数を指定して実行した場合は、常に不可視に変更する。"
	(interactive "P")
	(let ((sw flag))
		(if sw
				;; 強制的に invisible に。
				(progn
					(toggle-truncate-lines 0)
					(set 'timestamp-mode-string timestamp-mode-string-default)
					(add-to-invisibility-spec 'shell-timestamp))
			(if (member 'shell-timestamp buffer-invisibility-spec)
					;; invisible -> visible
					(progn
						(toggle-truncate-lines 1)
						(set 'timestamp-mode-string (concat timestamp-mode-string-default
																								timestamp-mode-string-suffix))
						(remove-from-invisibility-spec 'shell-timestamp))
				;; visible -> invisible
				(toggle-truncate-lines 0)
				(set 'timestamp-mode-string timestamp-mode-string-default)
				(add-to-invisibility-spec 'shell-timestamp)))
		;; redisplay
		(signal 'quit nil)))

(defun timestamp-insert-stamp-only-beginning (&optional flag)
	"タイムスタンプの挿入を行頭部分に限定するか否かを交互に切り替える。
通常では、サブプロセスからの応答状況により、応答の途中位置にタイムスタンプを
挿入することがあるが、`timestamp-insert-stamp-only-BOL-p' に nil 以外を指定
しておくと、行頭にしかタイムスタンプを挿入しない様に制限する。
このコマンドを実行することにより、行頭に制限する状態と制限しない状態を交互に
切り替える。
前置引数を指定して実行した場合は、常に行頭のみに制限する。"
	(interactive "P")
	(let ((sw flag))
		(if sw
				;; 強制的に行頭だけに。
				(set 'timestamp-insert-stamp-only-BOL-p t)
			(if timestamp-insert-stamp-only-BOL-p
					;; 行頭以外にも。
					(set 'timestamp-insert-stamp-only-BOL-p nil)
				;; 行頭だけ。
				(set 'timestamp-insert-stamp-only-BOL-p t)))
		(signal 'quit nil)))

(defun timestamp:kill-ring-save-wrapper (beg end &optional arg)
	"comint のバッファにタイムスタンプを挿入している状態で、それらの文字列を
不可視にしている場合は、`kill-ring-save' でリングバッファに取り込む文字列に
も不可視の文字列を含めないためのラッパー関数。

但し、挿入文字列を可視状態にしている場合は、それらの文字列もリングバッファに
取り込む。"
	(interactive "r")
	(if (and timestamp-mode
					 (member 'shell-timestamp buffer-invisibility-spec))
			(let ((string (buffer-substring-no-properties beg end))
						(overlays (overlays-in beg end))
						(base (1- beg))
						markers)
				(with-temp-buffer
					(insert string)
					(save-excursion
						(goto-char (point-min))

						;; 不可視として設定した overlay を選別し、位置をリストに格納。
						(while overlays
							(let* ((ovl (car overlays)))
								(when (overlay-get ovl 'invisible)
									(let ((start (- (overlay-start ovl) base))
												(end (- (overlay-end ovl) base)))
										;; delete すると位置が変化するので marker にして格納。
										(set 'markers (cons (cons
																					 (set-marker (make-marker) start)
																					 (set-marker (make-marker) end))
																					markers))))
								(set 'overlays (cdr overlays))))

						;; 対象の overlay から採取した markers を元にして削除する。
						(let ((posisions markers))
							(while posisions
								(let* ((pos (car posisions))
											 (start (car pos))
											 ;; overlay の end は、対象範囲外の場合があるので調整。
											 (end (min (cdr pos) (point-max))))
									(delete-region start end)
									(set 'posisions (cdr posisions))))))

					;; 一時バッファを `kill-ring-save' に渡す。
					(let ((beg (point-min))
								(end (point-max)))
						(kill-ring-save beg end))))

		;; 可視状態なら普通に `kill-ring-save' を呼び出す。
		(kill-ring-save beg end)))

(defvar timestamp-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map "\M-w" 'timestamp:kill-ring-save-wrapper)
		(define-key map "\C-cn" 'ow:comint-notify-enable)
		(define-key map "\C-cv" 'timestamp-insert-stamp-visible)
		map))

(define-minor-mode timestamp-mode
	"comint-mode のバッファにタイムスタンプを挿入するマイナーモード。"
  :global nil														; buffer local.
  :group 'comint-toolbox
  :init-value nil
	:keymap timestamp-mode-map

  (if timestamp-mode
      (progn
				(make-local-hook 'comint-output-filter-functions)
				(add-hook 'comint-output-filter-functions 'timestamp-insert-stamp t t)

				(set (make-local-variable 'timestamp-insert-stamp-start-mark)
						 (make-marker))
				;; (set (make-local-variable 'timestamp-mode) t)
				(set (make-local-variable 'timestamp-mode-string)
						 timestamp-mode-string-default)
				(or (assq 'timestamp-mode minor-mode-alist)
						(setq minor-mode-alist
									(cons '(timestamp-mode timestamp-mode-string)
												minor-mode-alist)))

				(set (make-local-variable 'ow:comint-notify-active-p) nil)
				(set (make-local-variable 'timestamp-insert-stamp-only-BOL-p)
						 timestamp-insert-stamp-only-BOL-p)

				(set-marker timestamp-insert-stamp-start-mark (point-min-marker))
				(add-to-invisibility-spec 'shell-timestamp))
		(remove-hook 'comint-output-filter-functions 'timestamp-insert-stamp t)
    (kill-local-variable 'timestamp-insert-stamp-start-mark)
    ;; (kill-local-variable 'timestamp-mode)
    (kill-local-variable 'timestamp-insert-stamp-only-BOL-p)
		(remove-from-invisibility-spec 'shell-timestamp)))

(when timestamp-mode-enable
	(add-hook
	 'comint-mode-hook
	 '(lambda ()
			(when (and (processp (get-buffer-process (current-buffer)))
								 ;; インタラクティブなバッファに限定。
								 (or (eq major-mode 'shell-mode)
										 (eq major-mode 'ssh-mode)))
				(timestamp-mode)))))

(provide 'timestamp-mode)
;;; timestamp-mode.el ends here
