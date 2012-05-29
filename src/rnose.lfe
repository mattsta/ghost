(defmodule rnose
 (export all))

(eval-when-compile
 ; turn anything reasonable into an atom
 (defun a
  ((c) (when (is_list c)) (list_to_atom c))
  ((c) (when (is_atom c)) c)
  ((c) (when (is_binary c)) (a (binary_to_list c))))

 ; turn anything reasonable into a list
 (defun l
  ((c) (when (is_list c)) c)
  ((c) (when (is_atom c)) (atom_to_list c))
  ((c) (when (is_binary c)) (binary_to_list c)))

 (defun mk-a (c d)
  (a (: lists flatten (cons (l c) (l d)))))
)

(defmacro zs-call
 ([fn redis-name args]
  `(defun ,fn ,args
    (: nose ,fn ',redis-name ,@args))))

(defmacro zs-call-t
 ([fn redis-name type-name args]
  `(defun ,(mk-a (mk-a type-name '-) fn) ,args
    (: nose ,fn ',redis-name ',type-name ,@args))))

(defmacro mk-nose-tied-to-redis-name
 ([redis-name]
  `(progn
    (zs-call object-create ,redis-name (type owner-uid))
    (zs-call object-create ,redis-name (type owner-uid hash-keys-vals))
    (zs-call object-update ,redis-name (type object-id key val))
    (zs-call object-update ,redis-name (type object-id hash-keys-vals))
    (zs-call name-new ,redis-name (target name))
    (zs-call name-new ,redis-name (type id name))
    (zs-call name-modify ,redis-name (type id name))
    (zs-call name-modify ,redis-name (target name))
    (zs-call name-delete ,redis-name (name))
    (zs-call object-tag-add ,redis-name (type object-id category tag))
    (zs-call object-tag-del ,redis-name (type object-id category tag))
    (zs-call owner-add ,redis-name (type object-id owner-uid))
    (zs-call owner-del ,redis-name (type object-id owner-uid))
    (zs-call object-all ,redis-name (type object-id))
    (zs-call object-field ,redis-name (type object-id key))
    (zs-call name-target ,redis-name (name))
    (zs-call category-tag-objects ,redis-name (type category tag))
    (zs-call category-tag-objects-count ,redis-name (type category tag))
    (zs-call object-categories ,redis-name (type object-id))
    (zs-call object-category-tags ,redis-name (type object-id category))
    (zs-call object-category-tags-count ,redis-name (type object-id category))
    (zs-call owns-objects ,redis-name (type uid))
    (zs-call object-owners ,redis-name (type object-id))
    (zs-call uid-owns-object ,redis-name (type uid object-id))
    (zs-call object-owned-by-uid ,redis-name (type object-id uid)))))

(defmacro mk-nose-tied-to-redis-name-type
 ([redis-name type]
  `(progn
    (zs-call-t object-create ,redis-name ,type (owner-uid hash-keys-vals))
    (zs-call-t object-update ,redis-name ,type (object-id key val))
    (zs-call-t object-update ,redis-name ,type (object-id hash-keys-vals))
    (zs-call-t name-new ,redis-name ,type (id name))
    (zs-call-t name-modify ,redis-name ,type (id name))
    (zs-call-t object-tag-add ,redis-name ,type (object-id category tag))
    (zs-call-t object-tag-del ,redis-name ,type (object-id category tag))
    (zs-call-t owner-add ,redis-name ,type (object-id owner-uid))
    (zs-call-t owner-del ,redis-name ,type (object-id owner-uid))
    (zs-call-t object-all ,redis-name ,type (object-id))
    (zs-call-t object-field ,redis-name ,type (object-id key))
    (zs-call-t category-tag-objects ,redis-name ,type (category tag))
    (zs-call-t category-tag-objects-count ,redis-name ,type (category tag))
    (zs-call-t object-categories ,redis-name ,type (object-id))
    (zs-call-t object-category-tags ,redis-name ,type (object-id category))
    (zs-call-t object-category-tags-count ,redis-name ,type
     (object-id category))
    (zs-call-t owns-objects ,redis-name ,type (uid))
    (zs-call-t object-owners ,redis-name ,type (object-id))
    (zs-call-t uid-owns-object ,redis-name ,type (uid object-id))
    (zs-call-t object-owned-by-uid ,redis-name ,type (object-id uid)))))

(mk-nose-tied-to-redis-name redis_nose)
(mk-nose-tied-to-redis-name-type redis_nose course)
(mk-nose-tied-to-redis-name-type redis_nose alo)
(mk-nose-tied-to-redis-name-type redis_nose clo)
