;;-----START(処理系の追加、ロードパスの追加、auto-installの設定)-----
;; clispをデフォルトのCommon Lisp処理系に設定
(setq inferior-lisp-program "/usr/local/bin/clisp")
;; ~/.emacs.d/slimeをload-pathに追加
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
;; SLIMEのロード
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))

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
(add-to-load-path "elisp" "conf" "public_repos")
;;ここにlandoflispのファイルとか、elpa,etc,infoも加えた方がいいかも。

;;package.elの設定
(when (require 'package nil t)
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
(when (require 'auto-install nil t)
  ;;インストールディレクトリを設定する
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;;EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;;必要であればプロキシの設定をする。
  ;;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;;install-elispの関数を利用可能にする。
  (auto-install-compatibility-setup))

;;-----END(処理系の追加、ロードパスの追加、auto-installの設定)-----



;;-----START(DB setting)-----

;;SQLサーバへ接続するためのデフォルト情報
;(setq sql-user "root" ;デフォルトユーザ名
;      sql-database "database_name" ;データベース名
;      sql-server "localhost" ;ホスト名
;      sql-product 'mysql) ;データベースの種類

;;-----END(DB setting)-----



;;-----START(General Settings)-----

;; yes-noをy-nに置き換え
(fset 'yes-or-no-p 'y-or-n-p)

;; M-xでコマンドを入力するときに候補を表示する
(icomplete-mode 1)

;; 行末の空白をハイライト
(setq-default show-trailing-whitespace t)

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

;;-----END(General Settings)-----



;;-----START(To serve Standard Function to Key-bind)-----

;;折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

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
    (egg rinari ctags inf-ruby ruby-block ruby-electric package-utils elscreen wgrep auto-complete helm multi-term htmlize))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;;GitフロントエンドEggの設定
(when (executable-find "git")
  (require 'egg nil t))

;;-----END(Settings of ELPA, melpa, and so on...)-----
