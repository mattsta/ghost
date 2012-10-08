(defmodule rghost
 (export all))

(defmacro zs-call
 ([fn redis-name args]
  `(defun ,fn ,args
    (: ghost ,fn ',redis-name ,@args))))

(defmacro mk-ghost-tied-to-redis-name
 ([redis-name]
  `(progn
    (zs-call object_create ,redis-name (object-id data-ptr))
    (zs-call object_create ,redis-name (object-id data-ptr data-contents))
    (zs-call object_parent ,redis-name (parent-id child-id))
    (zs-call object_weight_update ,redis-name (parent-id child-id delta))
    (zs-call object_rename ,redis-name (parent-id old-ch-id child-id))
    (zs-call object_top_n_children ,redis-name (object-id n))
    (zs-call object_children ,redis-name (object-id))
    (zs-call object_resolve_to_height ,redis-name (object-id height))
    (zs-call recur-child-depth ,redis-name (parent-id child-ids-with-scores))
    (zs-call vote ,redis-name (diff parent-id child-id user-id))
    (zs-call vote ,redis-name (diff weight parent-id child-id user-id))
    (zs-call vote_total_object ,redis-name (child-id))
    (zs-call vote_total ,redis-name (parent-id child-id))
    (zs-call votes_by_object ,redis-name (parent-id child-id))
    (zs-call votes_updown ,redis-name (parent-id child-id))
    (zs-call votes_by_user ,redis-name (user-id)))))

(mk-ghost-tied-to-redis-name redis_ghost)
