repeatable-motion.el
====================

The rationale for this package is that it can be nice to have a somewhat obscure motion function that you call by name, or by a longer-than-normal key sequence.  But it would be nice to be able to repeat the function again with a single key.  So here's how it works:

    (require 'repeatable-motion)

    ;; This will return a new function that executes the original, and sets the
    ;; repeats to forward-version and backward-version.  It doesn't define anything.
    ;; If forward-version is nil it will use the original, but sometimes you want
    ;; the repeat to actually be different (IE isearch)
    (make-repeatable-motion original-motion forward-version backward-version)

    ;; This will define two functions: repeatable-fwd-foo and repeatable-bkwd-foo.
    ;; Useful for simple motions that have opposites.
    (define-repeatable-pair 'fwd-foo 'bkwd-foo)

After the above, you can run `M-x repeatable-fwd-foo` to go forword.  Then run `M-x repeat-motion-forward` or `M-x repeat-motion-backward` to repeat as much as you want.  If the original call to the repeatable motion had a prefix argument, the repeats use the same one unless another one is given (except 1...).

Some common motions that I know have repeatable versions defined if the original ones are defined.  But I don't want to do any re-binds to not change peoples' workflows without their intervention.  If you think this is not a good idea, I might be convinced to change that.

Use cases
---------

If you are an Evil user, I recommend binding repeat-motion-forward and backward to the keys you would otherwise use for evil-repeat-find-char and evil-repeat-find-char-reverse (probably ; and ,).  I find that while I want to use evil-repeat-search with other motions in between, but I only repeat a find-char if I didn't get to the one I wanted first.

Personally, I have in my evil-normal-state two keys bound to prefix commands which are full of motions (forward for one, and backward for the other).  I try not to use forward-char and use more efficient motions, but on occasion it is easier to simply repeat forward-char.  This lets me lose the single-key binding to forward-char and just use a two-key binding, but then repeat it (or anything else) with a single key.

