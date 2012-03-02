(defmodule nose
 ; NOSE: New Object Storage Engine
 (export all))

;;;--------------------------------------------------------------------
;;; Keys
;;;--------------------------------------------------------------------
; list of all objects of type from oldest to newest
(defsyntax key-type-objects
 ([type] (: eru er_key 'nose type)))

; hash of properties for type:id
(defsyntax key-object-hash
 ([type object-id] (: eru er_key 'nose type 'id object-id)))

; for exposing keys to the outside world
(defsyntax key-external
 ([type object-id] (: eru er_key type object-id)))

; set of categories on an object
(defsyntax key-object-categories
 ([type object-id] (: eru er_key 'nose type 'id object-id 'cats)))

; set of names on an object
(defsyntax key-object-names
 ([type object-id] (: eru er_key 'nose type 'id object-id 'names)))

; set of admins/owners for an object
(defsyntax key-admins-for-object
 ([type object-id] (: eru er_key 'nose type 'id object-id 'admins)))

; type:id:category -> set of tags for this object in this category
(defsyntax key-type-category-to-tag-map
 ([type object-id category]
  (: eru er_key 'nose type 'id object-id 'tag (lccs category))))

; type:category:tag -> set of objects with this category:tag
(defsyntax key-type-tag-to-object-map
 ([type category in-tag]
  (: eru er_key 'nose type 'tag (lccs category) (lccs in-tag))))

; type:category -> set of tags in this category of this type
(defsyntax key-type-tags
 ([type category] (: eru er_key 'nose type 'tags (lccs category))))

(defsyntax lccs ([x] (lower-case-then-collapse-spaces x)))
; store all tags as lowercase with limited spacing
(defun lower-case-then-collapse-spaces
 ([tag] (when (is_list tag))
  (iolist_to_binary
   (: re replace (: string to_lower tag) '" +" '" " '(global))))
 ([tag] (when (is_binary tag))
  (lower-case-then-collapse-spaces
   (: unicode characters_to_list (binary_to_list tag))))
 ([tag] (when (is_atom tag))
  (lower-case-then-collapse-spaces (atom_to_binary tag 'utf8))))

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
          (reserve-name-finalize redis name (key-external type new-id))
          (object-update redis type new-id 'name name)
          (type-add-object redis type new-id)
          new-id))
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
(defun object-tag-add (redis type object-id category tag)
 ; add id to type:category:tag -> map of objects with this category:tag
 (: er sadd redis (key-type-tag-to-object-map type category tag) object-id)
 ; add tag to type:category -> map of all tags for this category
 (: er sadd redis (key-type-tags type category) (lccs tag))
 ; add category to type:id -> map of all categories for this object
 (: er sadd redis (key-object-categories type object-id) (lccs category))
 ; add tag to type:id:category -> map of all tags in this cat on this object
 (: er sadd redis (key-type-category-to-tag-map type object-id category)
  (lccs tag)))

; remove a tag from an object
(defun object-tag-del (redis type object-id category tag)
 ; remove object from global type:category:tag membership
 (: er srem redis (key-type-tag-to-object-map type category tag) object-id)
 ; remove tag from object's category
 (: er srem redis (key-type-category-to-tag-map type object-id category)
  (lccs tag))
 ; if object has no remainaing category members of this type, remove category
 (case (: er scard redis (key-type-category-to-tag-map type object-id category))
  ; if the object has no tags in this category, remove the category from obj
  (0 (: er srem redis (key-object-categories type object-id) (lccs category)))
  (_ 'ok)))

; add an owner to an object (update Owner->OBJs map and OBJ->Owners map)
(defun owner-add (redis type object-id owner-uid)
 ; add to OWNER->Objects set
 ; we don't need fully qualified keys in the set since we store by type
 (: er sadd redis (key-owner-admins-objects type owner-uid) object-id)
 ; add to OBJECT->Owners set
 (: er sadd redis (key-admins-for-object type object-id) owner-uid))

; remove an owner from an object (with caveat of can't be last owner removed)
(defun owner-del (redis type object-id owner-uid)
 (let ((owner-key (key-admins-for-object type object-id)))
  (case (: er scard redis owner-key) ; mild race condition.  we'll live.
   (1 'not_removed_you_are_last_owner)
   (_
    ; remove from OWNER->Objects Owned set
    ; we don't need fully qualified keys in the set since we store by type
    (: er srem redis (key-owner-admins-objects type owner-uid) object-id)
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

; get all objects for a category:tag
(defun category-tag-objects (redis type category tag)
 (: er smembers redis (key-type-tag-to-object-map type category tag)))

; count objects with category:tag
(defun category-tag-objects-count (redis type category tag)
 (: er scard redis (key-type-tag-to-object-map type category tag)))

; get all categories for an object
(defun object-categories (redis type object-id)
 (: er smembers redis (key-object-categories type object-id)))

; get all tags for a type:id:category
(defun object-category-tags (redis type object-id category)
 (: er smembers redis (key-type-category-to-tag-map type object-id category)))

; count objects for category
(defun object-category-tags-count (redis type object-id category)
 (: er scard redis (key-type-category-to-tag-map type object-id category)))

; get all objects by OWNER
(defun owns-objects (redis type uid)
 (: er smembers redis (key-owner-admins-objects type uid)))

; get all owners for OBJECT
(defun object-owners (redis type object-id)
 (: er smembers redis (key-admins-for-object type object-id)))

; check if user owns one object  (UID -> Ownership mapping test)
(defun uid-owns-object (redis type uid object-id)
 (: er sismember redis (key-owner-admins-objects type uid) object-id))

; check of object has one owner  (Object Ownership -> UID mapping)
(defun object-owned-by-uid (redis type object-id uid)
 (: er sismember redis (key-admins-for-object type object-id) uid))

;;;--------------------------------------------------------------------
;;; Overall Type Management
;;;--------------------------------------------------------------------
; add a new object to the list of all objects of its type
(defun type-add-object (redis type object-id)
 ; we're storing only the type object-id since we are listing by type
 ; it'll save us strlen("racl:TYPE:") bytes per entry by not storing the
 ; fully qualfied key (which shouldn't be exposed outside of this module anyway)
 (: er rpush redis (key-type-objects type) object-id))

(defun type-objects (redis type)
 (type-objects redis type 0 -1))

(defun type-objects (redis type offset count)
 (: er lrange redis (key-type-objects type) offset (+ offset count)))

(defun type-object-count (redis type)
 (: er llen redis (key-type-objects type)))
