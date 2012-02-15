(defmodule rnose
 (export all))

(defmacro zs-call
 ([fn redis-name args]
  `(defun ,fn ,args
    (: nose ,fn ',redis-name ,@args))))

(defmacro mk-nose-tied-to-redis-name
 ([redis-name]
  `(progn
    (zs-call object-create ,redis-name
     (type name owner-uid hash-keys-vals))
    (zs-call object-update ,redis-name (type object-id key val))
    (zs-call object-update ,redis-name (type object-id hash-keys-vals))
    (zs-call name-update ,redis-name (name new-target))
    (zs-call name-expire ,redis-name (name))
    (zs-call tag-add ,redis-name (type object-id tag))
    (zs-call tag-del ,redis-name (type object-id tag))
    (zs-call owner-add ,redis-name (type object-id owner-uid))
    (zs-call owner-del ,redis-name (type object-id owner-uid))
    (zs-call object-all ,redis-name (type object-id))
    (zs-call object-field ,redis-name (type object-id key))
    (zs-call name-target ,redis-name (name))
    (zs-call tag-members ,redis-name (type tag))
    (zs-call object-tags ,redis-name (type object-id))
    (zs-call owns-objects ,redis-name (type uid))
    (zs-call object-owners ,redis-name (type object-id)))))

(mk-nose-tied-to-redis-name redis_nose)
