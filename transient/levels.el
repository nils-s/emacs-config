;; Make the "sign commit" option (-S) available by default in Magit's commit dialog,
;; by setting its transient level to 4 (instead of its default value of 5).
;; Alternatively, the default transient level could be raised to 5 (instead of the
;; default value of 4), which would show all transient commands with a level of 5
;; or less.
;; For more details, see the transient docs, e.g.
;; https://magit.vc/manual/transient/Enabling-and-Disabling-Suffixes.html
((magit-commit
  (magit:--gpg-sign . 4)))
