;;-----START(処理系の追加、ロードパスの追加、auto-installの設定)-----
;; slimeこっちのPCで使える準備していないから、設定をコメントアウトする
;; clispをデフォルトのCommon Lisp処理系に設定
;;(setq inferior-lisp-program "/usr/local/bin/clisp")
;; ~/.emacs.d/slimeをload-pathに追加
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
;; SLIMEのロード
;;(require 'slime)
;;(slime-setup '(slime-repl slime-fancy slime-banner))

;;ロードパスを追加する。
(when (> emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))
;;load-pathを追加する関数を定義。
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))
;;引数のディレクトリとそのサブディレクトリをload-pathに追加。
(add-to-load-path "elisp")
;;ここにlandoflispのファイルとか、elpa,etc,infoも加えた方がいいかも。

;;C sourceのディレクトリを指定
;;(setq find-function-C-source-directory "~/Library/Caches/Homebrew/mituharu-emacs-mac-892fa7b2501a/src")


;;Package.Elの設定
;;M-x package-list-packages
(when (require 'package nil t)
  ;;パッケージをインストールする時以外は、基本的に速度のためにコメントアウトしておく
  ;;パッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  ;;インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

;;auto-installの設定
;;(when (require 'auto-install nil t)
  ;;パッケージをインストールする時以外は、基本的に速度のためにコメントアウトしておく
  ;;インストールディレクトリを設定する
  ;;(setq auto-install-directory "~/.emacs.d/elisp/")
  ;;EmacsWikiに登録されているelispの名前を取得する
  ;;(auto-install-update-emacswiki-package-name t)
  ;;必要であればプロキシの設定をする。
  ;;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;;install-elispの関数を利用可能にする。
  ;;(auto-install-compatibility-setup))

;;-----END(処理系の追加、ロードパスの追加、auto-installの設定)-----



;;-----START(Change save directory auto-save-file and back-up-file))-----
;; create backup file in ~/.emacs.d/backup
(setq make-backup-files t)
(setq backup-directory-alist
  (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
    backup-directory-alist))

;; create auto-save file in ~/.emacs.d/backup
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))

;;-----END(Change save directory auto-save-file and back-up-file))-----




;;-----START(DB setting)-----

;;SQLサーバへ接続するためのデフォルト情報
;(setq sql-user "root" ;デフォルトユーザ名
;      sql-database "database_name" ;データベース名
;      sql-server "localhost" ;ホスト名
;      sql-product 'mysql) ;データベースの種類

;;-----END(DB setting)-----



;;-----START(file-type-hook Settings)-----

;;.js.erb拡張子のファイルを開いた際に、js-modeがフックされるように設定
(add-to-list 'auto-mode-alist '("\\.js.erb$" . js-mode))
;;.haml拡張子のファイルを開いた際に、haml-modeがフックされるように設定
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;;React用
;;(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '(".*\\.js[x]?$" . rjsx-mode))
(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;;インデントはタブではなくスペース
            (setq js-indent-level 2) ;;スペースは２つ、デフォルトは4
            (setq js2-strict-missing-semi-warning nil))) ;;行末のセミコロンの警告はオフ
;;.yml, .yaml拡張子のファイルを開いた際に、yaml-modeがフックされるように設定
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(require 'slim-mode)
(add-hook 'slim-mode-hook
          '(lambda ()
             ;; C-x C-i からの左右インデント幅を slim-mode のインデント幅に合わせる
             (setq tab-width slim-indent-offset)))
;;JavaScriptモードでのインデントの幅をスペース二個分に修正(デフォルトは4個)
(add-hook 'js-mode-hook
	  (lambda ()
	    (make-local-variable 'js-indent-level)
	    (setq js-indent-level 2)))
;;Reactを書く際に、.jsや.jsxはweb-modeで編集するように設定(web-modeの細かな設定も記述)
;; .js, .jsx を web-mode で開く
;;(add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
;; .scssでもweb-modeで開く
(add-to-list 'auto-mode-alist '("\\.sass$" . web-mode))
;; .htmlでweb-modeを有効にする
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
;; 拡張子 .js でもJSX編集モードに
;;(setq web-mode-content-types-alist
      ;;'(("jsx" . "\\.js[x]?\\'")))

;;typescript関連
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq company-tooltip-align-annotations t)
(setq typescript-indent-level 2)
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
;;TSX
(require 'flycheck)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(flycheck-add-mode 'typescript-tslint 'web-mode)


;; インデント
(add-hook 'web-mode-hook
          (lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
          ))
;; ハイライト
(setq web-mode-enable-current-element-highlight t)
;; 色
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-comment-face ((t (:foreground "#587F35"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-css-pseudo-class ((t (:foreground "#DFCF44"))))
 '(web-mode-css-selector-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-string-face ((t (:foreground "#D78181"))))
 '(web-mode-doctype-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "#FFFFFF"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#D78181"))))
 '(web-mode-html-tag-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-server-comment-face ((t (:foreground "#587F35")))))
;;閉じタグ関連
(setq web-mode-auto-close-style 2)
(setq web-mode-tag-auto-close-style 2)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)

;;rainbow-mode設定
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'less-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;;css-mode(cssm-mirror-mode)
(defun css-mode-hooks ()
  "css-mode hooks"
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (setq cssm-indent-level 2)
  (setq css-indent-offset 2)
  (setq-default indent-tabs-mode nil)
  (setq cssm-newline-before-closing-bracket t))
(add-hook 'css-mode-hook 'css-mode-hooks)

;;-----END(file-type-hook Settings)-----



;;-----START(General Settings)-----

;; yes-noをy-nに置き換え
(fset 'yes-or-no-p 'y-or-n-p)

;; M-xでコマンドを入力するときに候補を表示する
(icomplete-mode 1)

;; 行末の空白をハイライト
(setq-default show-trailing-whitespace t)

;;一行ずつスクロールする
(setq scroll-step 1)

;;時計を表示
(display-time-mode 1)

;;バッテリー残量を表示
(display-battery-mode t)

;;タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;;hogehoge
(global-font-lock-mode t)

;;hogehoge
(show-paren-mode 1)

;;Rubyのマジックコメント(# coding: utf-8)の自動出力を抑止
(setq ruby-insert-encoding-magic-comment nil)

;;'¥' -> '\'
(define-key global-map [?¥] [?\\])

;;ignore error sound and flash
(setq ring-bell-function 'ignore)

;;-----END(General Settings)-----



;;-----START(To serve Standard Function to Key-bind)-----

;;折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
;;grep-find
(define-key global-map (kbd "M-g") 'grep-find)
;;コピーと切り取りのキーバインドを入れ替える
(define-key global-map (kbd "M-w") 'kill-region)
(define-key global-map (kbd "C-w") 'kill-ring-save)
;;org-twbs-export-to-html
;;org-modeの最高な出力コマンド
(define-key global-map (kbd "M-p") 'org-twbs-export-to-html)

;;-----END(To serve Standard Function to Key-bind)-----



;;-----START(Settings of auto-install)-----
;;注意事項
;;http通信ではインストールできなくなった。必ず、https通信にすること。

;;redo+の設定
;;https://www.emacswiki.org/emacs/download/redo+.el
(when (require 'redo+ nil t)
  ;;C-' にリドゥを割り当てる(C-'は動作しない)
  ;;(global-set-key (kbd "C-'") 'redo)
  ;;C-x uに近い、C-x 7を割り当てる。
  (global-set-key (kbd "C-x 7") 'redo)
  )

;;-----END(Settings of auto-install)-----



;;-----START(Settings of ELPA, melpa, and so on...)-----
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company typescript tide lsp-mode graphql-mode docker docker-compose-mode dockerfile-mode php-mode markdown-mode vue-html-mode vue-mode rjsx-mode slim-mode coffee-mode xpm csv-mode rainbow-mode magit xclip web-mode ox-twbs haml-mode egg rinari ctags inf-ruby ruby-block ruby-electric package-utils elscreen wgrep auto-complete helm multi-term htmlize))))


;;Helmの設定
(require 'helm-config)
(helm-mode 1)

;;auto-completeの設定
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
	       "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;;wgrepの設定
(require 'wgrep nil t)

;;ELScreenの設定
;;; プレフィクスキーはC-z
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)
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

;;hown(メモ機能)のインストールは保留

;;GitフロントエンドEggの設定(こいつ使っていない)
(when (executable-find "git")
  (require 'egg nil t))


;;magitの設定
(global-set-key (kbd "C-x g") 'magit-status)


;;multi-termの設定
(when (require 'multi-term nil t)
  ;;使用するシェルを指定
  (setq multi-term-program "/bin/bash"))


;;Settings of eww
;;key-bindingsの追加
;(define-key eww-mode-map "r" 'eww-reload)
;(define-key eww-mode-map "c 0" 'eww-copy-page-url)
;(define-key eww-mode-map "p" 'scroll-down)
;(define-key eww-mode-map "n" 'scroll-up)
;;背景色の設定
(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
;;(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
;;(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  "eww で文字色を反映させない"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  "eww で文字色を反映させる"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))
;;defaultの検索エンジンをgoogleにする
(setq eww-search-prefix "http://www.google.co.jp/search?q=")
(require 'docker)
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(require 'docker-compose-mode)

;;-----END(Settings of ELPA, melpa, and so on...)-----



;;-----START(Self-making commands)-----
;;(defun insert-title-format ()
;;  "現在バッファに----------を表示し、ポイントをその真ん中に移動する。"
;;  (interactive)
;;  (progn
;;    (insert "----------")
;;    (goto-char (- (point) 5))))
;;org-modeのフォーマットに合わせた方が可読性が上がるため、上記はコメントアウト
(defun insert-title-format ()
  "現在バッファに#+TITLE: と、見出し1を挿入し、カーソルを:の後ろに移動する。"
  (interactive)
  (insert "#+TITLE: ")
  (newline)
  (insert "#+OPTIONS: ^:{}")
  (newline)
  (insert "* ")
  (previous-line)
  (previous-line)
  (move-end-of-line 1))
(define-key global-map (kbd "C-t") 'insert-title-format)
;;C-t runs the command transpose-chars (found in global-map) for the defult.

;;C-z g
(defun insert-onclick-ga ()
  "GAタグの埋め込み"
  (interactive)
  (progn
    (insert "onclick: \"ga('send', 'event', '', '')\"")
    (goto-char (- (point) 7))))
(define-key global-map (kbd "C-z g") 'insert-onclick-ga)

(defun insert-quoted-comma ()
  "現在バッファに\"\",を表示し、ポイントを\"\"の間に移動する。"
  (interactive)
  (progn
    (insert "\"\",")
    (goto-char (- (point) 2))))
(define-key global-map (kbd "C-q") 'insert-quoted-comma)
;;C-q runs the command quoted-insert for the default.

;; insert binding.pry
(defun insert-binding-pry ()
  "railsデバッグ用"
  (interactive)
  (insert "binding.pry")
  (move-end-of-line 1))
(define-key global-map (kbd "C-x p") 'insert-binding-pry)

;; insert comma to sql result
(defun insert-comma-to-sql-query-result ()
  "SQL実行結果に対して、カンマを付与する。"
  (interactive)
  (progn
    (insert ",")
    (next-line)
    (end-of-line)
    ))
(define-key global-map (kbd "C-x ,") 'insert-comma-to-sql-query-result)
;;-----START(Self-making commands)-----
