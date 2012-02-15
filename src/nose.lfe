(defmodule nose
 ; NOSE: New Object Storage Engine
 (export all))

;;;--------------------------------------------------------------------
;;; Keys
;;;--------------------------------------------------------------------
; hash of properties for type:id
(defsyntax key-object-hash
 ([type object-id] (: eru er_key 'nose type 'id object-id)))

; set of tags on an object
(defsyntax key-object-tags
 ([type object-id] (: eru er_key 'nose type 'id object-id 'tags)))

; set of names on an object
(defsyntax key-object-names
 ([type object-id] (: eru er_key 'nose type 'id object-id 'names)))

; set of admins/owners for an objwct
(defsyntax key-admins-for-object
 ([type object-id] (: eru er_key 'nose type 'id object-id 'admins)))

; set of type:tag -> list of objects with that tag
(defsyntax key-type-tag-to-object-map
 ([type in-tag] (: eru er_key 'nose type 'tag in-tag)))

; set of all objects UID has admin rights on
(defsyntax key-owner-admins-objects
 ([type uid] (: eru er_key 'nose type 'admin uid)))

; counter of things of type
(defsyntax key-counter-nose-type-object-id
 ([in-type] (: eru er_key 'nose 'counter 'type type)))

; global NAME -> What lookup
(defsyntax key-name-ptr
 ([in-name] (: eru er_key 'nose 'name in-name)))

;;;--------------------------------------------------------------------
;;; Object Creation
;;;--------------------------------------------------------------------
(defun new-object-key (redis type)
 (let* ((new-id (: er incr redis (key-counter-nose-type-object-id type)))
        (new-key (key-object-hash type new-id)))
  (tuple new-id new-key)))

(defun object-create (redis type name owner-uid hash-keys-vals)
 ; we don't allow duplicate object ids
 (case (reserve-name redis name)
  ('true (let (((tuple new-id new-obj-key) (new-object-key redis type)))
          (: er hmset redis new-obj-key hash-keys-vals)
          (owner-add redis type new-id owner-uid)
          (reserve-name-finalize redis name new-obj-key)
          new-obj-key))
  ('false 'name_exists)))

(defun reserve-name (redis name)
 (case (: er setnx redis (key-name-ptr name) 'temp-holder)
  ('true
   (: er expire redis (key-name-ptr name) 5) ; del in 5s if not used/updated
   'true)
  ('false 'false)))

(defun reserve-name-finalize (redis name val)
 (name-update redis name val))

;;;--------------------------------------------------------------------
;;; Object Updating
;;;--------------------------------------------------------------------
; update a keyval in a hash
(defun object-update (redis type object-id key val)
 (: er hset redis (key-object-hash type object-id) key val))

; update keyvals in a hash
(defun object-update (redis type object-id hash-keys-vals)
 (: er hmset redis (key-object-hash type object-id) hash-keys-vals))

; update where a name points
(defun name-update (redis name new-target)
 (: er set redis (key-name-ptr name) new-target))

; mark a name as expired/nil/nowhere
(defun name-expire (redis name)
 (: er set redis (key-name-ptr name) '::removed::))

; add a tag to an object
(defun tag-add (redis type object-id tag)
 (: er sadd redis (key-type-tag-to-object-map type tag)
  (key-object-hash type object-id))
 (: er sadd redis (key-object-tags type object-id) tag))

; remove a tag from an object
(defun tag-del (redis type object-id tag)
 (: er srem redis (key-type-tag-to-object-map type tag)
  (key-object-hash type object-id))
 (: er srem redis (key-object-tags type object-id) tag))

; add an owner to an object (update Owner->OBJs map and OBJ->Owners map)
(defun owner-add (redis type object-id owner-uid)
 ; add to OWNER->Objects set
 (: er sadd redis (key-owner-admins-objects type owner-uid)
  (key-object-hash type object-id))
 ; add to OBJECT->Owners set
 (: er sadd redis (key-admins-for-object type object-id) owner-uid))

; remove an owner from an object (with caveat of can't be last owner removed)
(defun owner-del (redis type object-id owner-uid)
 (let ((owner-key (key-admins-for-object type object-id)))
  (case (: er scard redis owner-key) ; mild race condition.  we'll live.
   (1 'not_removed_you_are_last_owner)
   (_ 
    ; remove from OWNER->Objects Owned set
    (: er srem redis (key-owner-admins-objects type owner-uid)
     (key-object-hash type object-id))
    ; Remove from OBJECT->Owners set
    (: er srem redis owner-key owner-uid)))))

;;;--------------------------------------------------------------------
;;; Object Reading
;;;--------------------------------------------------------------------
; read an entire object
(defun object-all (redis type object-id)
 (: er hgetall_p redis (key-object-hash type object-id)))

; read a value from a key of an object
(defun object-field (redis type object-id key)
 (: er hget redis (key-object-hash type object-id) key))

; read where a name points
(defun name-target (redis name)
 (: er get redis (key-name-ptr name)))

; get all objects for a tag
(defun tag-members (redis type tag)
 (: er smembers redis (key-type-tag-to-object-map type tag)))

; get all tags for an object
(defun object-tags (redis type object-id)
 (: er smembers redis (key-object-tags type object-id)))

; get all objects by OWNER
(defun owns-objects (redis type uid)
 (: er smembers redis (key-owner-admins-objects type uid)))

; get all owners for OBJECT
(defun object-owners (redis type object-id)
 (: er smembers redis (key-admins-for-object type object-id)))
