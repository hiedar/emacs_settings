;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; BASIC ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; パッケージの設定
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;;; splash screenを無効にする
(setq inhibit-splash-screen t)

;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; C-u C-SPC C-SPC …でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;; Disable beep sound
(setq ring-bell-function 'ignore)

;;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
(require 'uniquify)

;;; リージョンを削除できるように
(delete-selection-mode t)

;;; ファイルを開いた位置を保存する
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacsからの質問をy/nで回答する
(fset 'yes-or-no-p 'y-or-n-p)

;;; 釣合う括弧をハイライトする
(show-paren-mode 1)

;;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)

;;; 現在行に色をつける
(global-hl-line-mode 1)

;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; シェルに合わせるため、C-hは後退に割り当てる
(global-set-key (kbd "C-h") 'delete-backward-char)

;;; Begining / End of buffer
(require 'sequential-command-config)
(sequential-command-setup-keys)

;;; モードラインに時刻を表示する
(display-time)

;;; 行番号・桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)

;;; 画面横に行番号を表示する
(require 'linum)
(global-linum-mode t)      ; デフォルトで linum-mode を有効にする
(setq linum-format "%5d ") ; 5 桁分の領域を確保して行番号のあとにスペースを入れる

;;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; ログの記録行数を増やす
(setq message-log-max 10000)

;;; 履歴をたくさん保存する
(setq history-length 1000)

;;; メニューバーとツールバーとスクロールバーを消す
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; バックアップの抑止(~がついたバックアップファイルを作らない)
(setq make-backup-files nil)

;;; font-face
(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 130)
;(set-face-attribute 'default nil
;                    :family "Menlo"
;                    :height 120)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
    (yaml-mode gradle-mode kotlin-mode php-mode ctags lua-mode dracula-theme redo+ sequential-command swift3-mode recentf-ext helm markdown-mode elscreen-persist elscreen smart-tab ace-jump-mode migemo)))
 '(tab-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;; theme ;;;;;;;;;;;;;;;;
(load-theme 'dracula t)
;; cursor type
(setq cursor-type 'hollow)
;; cursor color
;;(set-cursor-color 'sea-green)

;;; migemo
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (locate-library "migemo")
  (setq migemo-command "/usr/local/bin/cmigemo") ; HERE cmigemoバイナリ
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict") ; HERE Migemo辞書
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))

;;; ace-jump-mode
(require 'ace-jump-mode)
(setq ace-jump-mode-gray-background nil)
(setq ace-jump-word-mode-use-query-char nil)
(setq ace-jump-mode-move-keys
(append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))
(global-set-key (kbd "C-:") 'ace-jump-word-mode)

;;; window
(global-set-key (kbd "C-t") 'other-window)

;;; Ohter keybind.
;; M-k でカレントバッファを閉じる
(define-key global-map (kbd "M-k") 'kill-this-buffer)
;; Mac の Command + 3 でウィンドウを上下に分割
(define-key global-map (kbd "s-3") 'split-window-vertically)
;; Mac の command + 2 でウィンドウを左右に分割
(define-key global-map (kbd "s-2") 'split-window-horizontally)
;; Mac の Command + 1 で現在のウィンドウ以外を閉じる
(define-key global-map (kbd "s-1") 'delete-other-windows)
;; Mac の Command + 0 で現在のウィンドウを閉じる
(define-key global-map (kbd "s-0") 'delete-window)

;;; redo
(require 'redo+)
(global-set-key (kbd "C-M-/") 'redo)

;;; 改行やタブを可視化する whitespace-mode
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□]) ; zenkaku space
        (newline-mark 10 [8629 10]) ; newlne
        (tab-mark 9 [187 9] [92 9]) ; tab » 187
        )
      whitespace-style
      '(spaces
        ;; tabs
        trailing
        newline
        space-mark
        tab-mark
        newline-mark))
;; whitespace-modeで全角スペース文字を可視化　
(setq whitespace-space-regexp "\\(\x3000+\\)")
;; whitespace-mode をオン
(global-whitespace-mode t)

;;; PATH設定
;; http://sakito.jp/emacs/emacsshell.html#path
;; より下に記述した物が PATH の先頭に追加されます
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/sw/bin"
              "/usr/local/bin"
              "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
;; PATH と exec-path に同じ物を追加します
 (when (and (file-exists-p dir) (not (member dir exec-path)))
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (setq exec-path (append (list dir) exec-path))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elscreen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; プレフィクスキーはC-z
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)
(elscreen-persist-mode 1)
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)
; 新規スクリーン作成
(global-set-key (kbd "s-t") 'elscreen-create)
;; スクリーンとバッファをkill
(global-set-key (kbd "s-w") 'elscreen-kill-screen-and-buffers)
;;ブラウザみたいにタブ移動
(global-set-key [(C-tab)] 'elscreen-next)
(global-set-key [(M-s-right)] 'elscreen-next)
(global-set-key [(C-S-tab)] 'elscreen-previous)
(global-set-key [(M-s-left)] 'elscreen-previous)
;; diredを新しいscreenで
(global-set-key (kbd "C-x C-d") 'elscreen-dired)
;; elscreenで開いているバッファの一覧を表示
(global-set-key (kbd "C-t") 'helm-elscreen)

;;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

;;; ffap
(ffap-bindings)

;;; recentf関連
;; 最近のファイル500個を保存する
(setq recentf-max-saved-items 500)
;; 最近使ったファイルに加えないファイルを
;; 正規表現で指定する
(setq recentf-exclude
      '("/TAGS$" "/var/tmp/"))
(require 'recentf-ext)

;;; helm
(require 'helm-config)
(helm-mode 1)
(helm-migemo-mode 1)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)

;; C-hで前の文字削除
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'helm-mini)
(define-key isearch-mode-map (kbd "C-h") 'isearch-del-char)

;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; for ctags.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ctags nil t)
(setq tags-revert-without-query t)
(setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)
(global-set-key (kbd "M-.") 'ctags-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Markdown mode with Marked 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://qiita.com/gooichi/items/2b185dbdf24166a15ca4

(setq markdown-open-command "marked2")
