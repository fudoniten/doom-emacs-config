;;; site.d/kubernetes.el -*- lexical-binding: t; -*-

;; kubernetes-el: a magit-style overview of the current kubectl context.
;; It shells out to kubectl with ~/.kube/config, which fudo-nix-home's
;; `fudo.kube' writes on hosts that have a cluster (seattle-infra on
;; sea.fudo.org, thecitadel-infra on burg.fudo.org).

;;; Code:

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  ;; The overview polls every resource type on a timer by default, which is
  ;; a steady stream of API calls for a buffer mostly left open in the
  ;; background.  Refresh by hand (g) instead.
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(provide 'kubernetes-config)
;;; kubernetes.el ends here
