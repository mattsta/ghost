(defmodule cghost
 (export all))

(include-file "include/ghost-module-maker.lfe")

(mk-ghost-tied-to-redis-name redis_chatty_ghost)
