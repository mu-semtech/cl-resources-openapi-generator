(in-package :openapi-generator)

(defun all-resources ()
  (loop for val being
     the hash-values of mu-cl-resources::*resources*
     collect val))

(defun generate ()
  "Construct full generation."
  (let ((spec
         (jsown:new-js
           ("swagger" "2.0")
           ("info"
            (jsown:new-js ("title" "generated mu-cl-resources api")
                          ("description" "API generated from mu-cl-resources for a specific domain")
                          ("version" "1.0.0")))
           ("schemes" (list "http"))
           ("basePath" "/")
           ("produces" (list "application/vnd.api+json"))
           ("paths" (merge-jsown-objects
                     (mapcar #'generate-resource (all-resources))))
           ("definitions" (merge-jsown-objects
                               (mapcar #'generate-single-resource-definition
                                       (all-resources)))))))
    (format t "~A" (jsown:to-json spec))))

(defun merge-jsown-objects (objects-to-merge)
  "Builds one big object from the collection of the list of 
   supplied objects.  Similar to the flattening of a list."
  (let ((result (jsown:empty-object)))
    (loop for spec in objects-to-merge
       do (loop for key in (jsown:keywords spec)
             do (setf (jsown:val result key)
                      (jsown:val spec key))))
    result))

(defun simple-query (query)
  "Sends a simple query and returns the first result.
   The result's parameter should be ?result."
  (let ((result (first (sparql:select (s-var "result")
                                      query))))
    (when result
      (jsown:filter "result" "value"))))

(defun triple-pattern (subject predicate object)
  "Returns the triple pattern for the given subject, predicate and object."
  (format nil "~A ~A ~A." subject predicate object))

(defun triple-pattern-query (subject predicate object)
  "Queries the triplestore for an s-p-o pattern.  Expects the sparql
   variable to be ?result."
  (simple-query (triple-pattern subject predicate object)))

(defun generate-resource (resource)
  "Returns the generator for the specific resource"
  (merge-jsown-objects (list (generate-resource-list-calls resource)
                             (generate-resource-object-calls resource)
                             (generate-resource-link-calls resource))))

(defun generate-resource-list-calls (resource)
  "Returns the jsown object containing the list calls which can be done
   on the supplied resource."
  (let ((path (format nil "/~A" (mu-cl-resources::request-path resource))))
    (jsown:new-js
      (path
       (jsown:new-js
         ("get" (jsown:new-js
                  ("summary" (resource-label-for-listing resource))
                  ("description" (resource-description-for-listing resource))
                  ("parameters" (resource-parameters-for-listing resource))
                  ("tags" (resource-tags-for-listing resource))
                  ("responses" (resource-responses-for-listing resource))))
         ("post" (jsown:new-js
                   ("summary" (resource-label-for-create resource))
                   ("description" (resource-description-for-create resource))
                   ("parameters" (resource-parameters-for-create resource))
                   ("tags" (resource-tags-for-create resource))
                   ("responses" (resource-responses-for-create resource)))))))))

(defun generate-resource-object-calls (resource)
  "Returns the jsown object containing the calls which can be done
   on an instance of the supplied resource."
  (let ((id-path (format nil "/~A/{id}" (mu-cl-resources::request-path resource))))
    (jsown:new-js
      (id-path
       (jsown:new-js
         ("get" (jsown:new-js
                  ("summary" (resource-label-for-show resource))
                  ("description" (resource-description-for-show resource))
                  ("parameters" (resource-parameters-for-show resource))
                  ("tags" (resource-tags-for-show resource))
                  ("responses" (resource-responses-for-show resource))))
         ("patch" (jsown:new-js
                    ("summary" (resource-label-for-patch resource))
                    ("description" (resource-description-for-patch resource))
                    ("parameters" (resource-parameters-for-patch resource))
                    ("tags" (resource-tags-for-patch resource))
                    ("responses" (resource-responses-for-patch resource))))
         ("delete" (jsown:new-js
                     ("summary" (resource-label-for-delete resource))
                     ("description" (resource-description-for-delete resource))
                     ("parameters" (resource-parameters-for-delete resource))
                     ("tags" (resource-tags-for-delete resource))
                     ("responses" (resource-responses-for-delete resource)))))))))

(defun generate-resource-link-calls (resource)
  "Returns the jsown object containing the links calls which can be done
   on an instance of a resource."
  (let ((result (jsown:empty-object)))
    (loop for relationship in (mu-cl-resources::all-links resource)
       do (setf result
                (merge-jsown-objects
                 (list result
                       (generate-resource-link-calls-for-link resource relationship)))))
    result))

(defun generate-resource-link-calls-for-link (resource link)
  (let* ((resource-path (mu-cl-resources::request-path resource))
         (relationship-name (mu-cl-resources::json-property-name link))
         (short-path (format nil "/~A/{id}/~A" resource-path relationship-name))
         (long-path (format nil "/~A/{id}/links/~A" resource-path relationship-name)))
    (jsown:new-js (short-path
                   (jsown:new-js
                     ("get" (generate-resource-for-relation-get-call resource link))))
                  (long-path
                   (let ((base
                          (jsown:new-js
                            ("get" (generate-resource-for-relation-get-call resource link))
                            ("patch" (generate-resource-for-relation-patch-call resource link))))
                         (post-content (generate-resource-for-relation-post-call resource link))
                         (delete-content (generate-resource-for-relation-delete-call resource link)))
                     (when post-content
                       (setf (jsown:val base "post")
                             post-content))
                     (when delete-content
                       (setf (jsown:val base "delete")
                             delete-content))
                     base)))))

(defun generate-id-parameter-specification (resource)
  (declare (ignore resource))
  (jsown:new-js ("name" "id")
                ("in" "path")
                ("description" "uuid of the requested resource")
                ("required" t)
                ("type" "string")))

(defun generate-resource-for-relation-get-call (resource relationship)
  "Outputs the contents specifying the relation's get call"
  (jsown:new-js
    ("summary" (format nil "Lists ~A relationship."
                           (mu-cl-resources::json-property-name relationship)))
    ("description" (format nil "Lists ~A relationship.   Contains results for links named ~A of mu-cl-resource type ~A, coming from resource of mu-cl-resources type ~A."
                           (mu-cl-resources::json-property-name relationship)
                           (mu-cl-resources::json-property-name relationship)
                           (mu-cl-resources::json-type (mu-cl-resources::referred-resource relationship))
                           (mu-cl-resources::json-type resource)))
    ("parameters"
     (list (generate-id-parameter-specification resource)))
    ("tags" (list "relationship" "listing"
                  (string-type-name (mu-cl-resources::referred-resource relationship))
                  (string-type-name resource)))
    ("responses"
     (jsown:new-js
       ("200"
        (jsown:new-js
          ("description" (format nil "An array of instances of ~A"
                                 (mu-cl-resources::json-type
                                  (mu-cl-resources::referred-resource relationship))))
          ("schema"
           (jsown:new-js
             ("type" "object")
             ("properties"
              (jsown:new-js
                ("links" (jsown:new-js
                           ("type" "object")
                           ("description" "provides relevant links to fetch and update the relationship")
                           ("properties"
                            (jsown:new-js
                              ("self" (jsown:new-js ("type" "string")
                                                    ("description" "a link for the relationship itself (a \"relationship link\"). This link allows the client to directly manipulate the relationship.")))
                              ("related" (jsown:new-js ("type" "string")
                                                       ("description" "A \"related resource link\" provides access to resource objects linked in a relationship. When fetched, the related resource object(s) are returned as the response’s primary data.")))))))
                ("data" (generate-data-property-for-relationship resource relationship))))))))
       ;; TODO include error codes
       ))))

(defgeneric generate-data-property-for-relationship (resource relationship)
  (:documentation "generates the data property description for <relationship> of <resource>.")
  (:method (resource (relationship mu-cl-resources::has-one-link))
    ;; TODO should also include :null
    (jsown:new-js
      ("$ref" (ref-to-resource
               (mu-cl-resources::referred-resource relationship)))))
  (:method (resource (relationship mu-cl-resources::has-many-link))
    (jsown:new-js
      ("type" "array")
      ("items" (jsown:new-js
                 ("$ref" (ref-to-resource
                          (mu-cl-resources::referred-resource relationship))))))))

(defun generate-resource-for-relation-patch-call (resource relationship)
  "Outputs the contents specifying the relation's patch call"
  (jsown:new-js
    ("summary" (format nil "Updates ~A relationship."
                           (mu-cl-resources::json-property-name relationship)))
    ("description" (format nil "Updates ~A relationship.   Updates results for links named ~A of mu-cl-resource type ~A, coming from resource of mu-cl-resources type ~A."
                           (mu-cl-resources::json-property-name relationship)
                           (mu-cl-resources::json-property-name relationship)
                           (mu-cl-resources::json-type (mu-cl-resources::referred-resource relationship))
                           (mu-cl-resources::json-type resource)))
    ("parameters"
     (list (generate-id-parameter-specification resource)
           (generate-body-parameter-for-relation-patch-call resource relationship)))
    ("tags" (list "relationship" "update"
                  (string-type-name (mu-cl-resources::referred-resource relationship))
                  (string-type-name resource)))
    ("responses"
     (jsown:new-js
       ("204"
        (jsown:new-js
          ("description" (format nil "Relation ~A updated successfully"
                                 (mu-cl-resources::json-property-name relationship)))))
       ;; TODO include error codes
       ))))

(defgeneric generate-resource-for-relation-post-call (resource relationship)
  (:documentation "Outputs the contents specifying the relation's patch call")
  (:method (resource (relationship mu-cl-resources::has-one-link))
    nil)
  (:method (resource (relationship mu-cl-resources::has-many-link))
    (jsown:new-js
      ("summary" (format nil "Adds resources to the ~A relationship"
                         (mu-cl-resources::json-property-name relationship)))
      ("description" (format nil "Adds resources to ~A relationship.  Adds result for link named ~A of mu-cl-resources type ~A, coming from resource of mu-cl-resources type ~A"
                           (mu-cl-resources::json-property-name relationship)
                           (mu-cl-resources::json-property-name relationship)
                           (mu-cl-resources::json-type (mu-cl-resources::referred-resource relationship))
                           (mu-cl-resources::json-type resource)))
      ("parameters"
       (list (generate-id-parameter-specification resource)
                                        ; looks similar to patch for has-many-link
             (generate-body-parameter-for-relation-patch-call resource relationship)))
      ("tags" (list "relationship" "update"
                  (string-type-name (mu-cl-resources::referred-resource relationship))
                  (string-type-name resource)))
      ("responses"
       (jsown:new-js
         ("204"
          (jsown:new-js
            ("description" (format nil "Relation ~A updated successfully"
                                   (mu-cl-resources::json-property-name relationship)))))
         ;; TODO include error codes
         )))))

(defgeneric generate-resource-for-relation-delete-call (resource relationship)
  (:documentation "Outputs the contents specifying the relation's delete call")
  (:method (resource (relationship mu-cl-resources::has-one-link))
    nil)
  (:method (resource (relationship mu-cl-resources::has-many-link))
    (jsown:new-js
      ("summary" (format nil "Removes resources from the ~A relationship"
                         (mu-cl-resources::json-property-name relationship)))
      ("description" (format nil "Removes resource from ~A relationship.  Removes results for link named ~A of mu-cl-resources type ~A, coming from resource of mu-cl-resources type ~A"
                           (mu-cl-resources::json-property-name relationship)
                           (mu-cl-resources::json-property-name relationship)
                           (mu-cl-resources::json-type (mu-cl-resources::referred-resource relationship))
                           (mu-cl-resources::json-type resource)))
      ("parameters"
       (list (generate-id-parameter-specification resource)
                                        ; looks similar to patch for has-many-link
             (generate-body-parameter-for-relation-patch-call resource relationship)))
      ("tags" (list "relationship" "update"
                  (string-type-name (mu-cl-resources::referred-resource relationship))
                  (string-type-name resource)))
      ("responses"
       (jsown:new-js
         ("204"
          (jsown:new-js
            ("description" (format nil "Relation ~A updated successfully"
                                   (mu-cl-resources::json-property-name relationship)))))
         ;; TODO include error codes
         )))))



(defgeneric generate-body-parameter-for-relation-patch-call (resource relationship)
  (:documentation "generates the body praameter for the relation patch call")
  (:method (resource (relationship mu-cl-resources::has-one-link))
    (jsown:new-js
      ("name" "data")
      ("in" "body")
      ("description" "New relationship contents")
      ("required" t)
      ("schema"
       (jsown:new-js
         ("type" "object")
         ("properties"
          (jsown:new-js
            ("type"
             (jsown:new-js ("type" "string")
                           ("description"
                            (format nil
                                    "Type of the resource, always ~A for this call."
                                    (mu-cl-resources::json-type (mu-cl-resources::referred-resource relationship))))))
            ("id"
             (jsown:new-js ("type" "string")
                           ("description" "uuid of the newly connected resource")))))))))
  (:method (resource (relationship mu-cl-resources::has-many-link))
    (jsown:new-js
      ("name" "data")
      ("in" "body")
      ("description" "New relationship contents")
      ("required" t)
      ("schema"
       (jsown:new-js
         ("type" "array")
         ("description" "Array containing specifiers of the new relationship")
         ("items"
          (jsown:new-js
            ("properties"
             (jsown:new-js
               ("type"
                (jsown:new-js
                  ("type" "string")
                  ("description"
                   (format nil
                           "Type of the resource, always ~A for this call."
                           (mu-cl-resources::json-type (mu-cl-resources::referred-resource relationship))))))
               ("id"
                (jsown:new-js
                  ("type" "string")
                  ("description" "uuid of the newly connected resource"))))))))))))

(defun resource-label-for-delete (resource)
  "Returns the label for deleting a resource"
  (let ((label (triple-pattern-query (ld-class resource)
                                     (s-url "http://www.w3.org/2000/01/rdf-schema#label")
                                     (s-var "result"))))
    (if label
        (format nil "Deletes instance: ~A" label)
        (format nil "Deletes instance of type ~A" (string-type-name resource)))))

(defun resource-description-for-delete (resource)
  (let ((description
         (triple-pattern-query (ld-class resource)
                               (s-url "http://www.w3.org/2000/01/rdf-schema#comment")
                               (s-var "result"))))
    (if description
        (format nil "Deletes instance described as: ~A" description)
        (format nil "Deletes resource of type ~A" (string-type-name resource)))))

(defun resource-parameters-for-delete (resource)
  "Shows the parameters for deleting a single resource."
  (declare (ignore resource))
  (list (jsown:new-js ("name" "id")
                      ("in" "path")
                      ("description" "uuid of the requested resource")
                      ("required" t)
                      ("type" "string"))))

(defun resource-tags-for-delete (resource)
  "Returns a set of tags which could be used to group this resource"
  (list "non-relationship" "delete" (string-type-name resource)))

(defun resource-responses-for-delete (resource)
  "Returns the possible responses for the deletion of a resource"
  ;; TODO: make this right
  (jsown:new-js
    ("200" (jsown:new-js
             ("description" (format nil "Instance of ~A" (string-type-name resource)))
             ("schema" ;; TODO: this is not correct
              (jsown:new-js ("$ref" (format nil "#/definitions/~A"
                                            (string-type-name resource)))))))
    ("default" (jsown:new-js
                 ;; we should abide jsonapi, but for real errors we don't
                 ("description" "An error occurred")))))


(defun resource-label-for-patch (resource)
  "Returns the label for patching a resource"
  (let ((label (triple-pattern-query (ld-class resource)
                                     (s-url "http://www.w3.org/2000/01/rdf-schema#label")
                                     (s-var "result"))))
    (if label
        (format nil "Updates instance: ~A" label)
        (format nil "Updates instance of type ~A" (string-type-name resource)))))

(defun resource-description-for-patch (resource)
  (let ((description
         (triple-pattern-query (ld-class resource)
                               (s-url "http://www.w3.org/2000/01/rdf-schema#comment")
                               (s-var "result"))))
    (if description
        (format nil "Updates resource described as: ~A" description)
        (format nil "Updates resource of type ~A" (string-type-name resource)))))

(defun resource-parameters-for-patch (resource)
  "Shows the parameters for showing a single resource."
  (declare (ignore resource))
  (list (jsown:new-js ("name" "id")
                      ("in" "path")
                      ("description" "uuid of the requested resource")
                      ("required" t)
                      ("type" "string"))))

(defun resource-tags-for-patch (resource)
  "Returns a set of tags which could be used to group this resource"
  (list "non-relationship" "update" (string-type-name resource)))

(defun resource-responses-for-patch (resource)
  "Returns the possible responses for the patching of a resource"
  ;; TODO: make this right
  (jsown:new-js
    ("200" (jsown:new-js
             ("description" (format nil "Instance of ~A" (string-type-name resource)))
             ("schema" ;; TODO: this is not correct
              (jsown:new-js ("$ref" (format nil "#/definitions/~A"
                                            (string-type-name resource)))))))
    ("default" (jsown:new-js
                 ;; we should abide jsonapi, but for real errors we don't
                 ("description" "An error occurred")))))


(defun resource-label-for-show (resource)
  "Returns the label for showing a resource"
  (let ((label (triple-pattern-query (ld-class resource)
                                     (s-url "http://www.w3.org/2000/01/rdf-schema#label")
                                     (s-var "result"))))
    (if label
        (format nil "Retrieves instance: ~A" label)
        (format nil "Retrieves instance of type ~A" (string-type-name resource)))))

(defun resource-description-for-show (resource)
  (let ((description
         (triple-pattern-query (ld-class resource)
                               (s-url "http://www.w3.org/2000/01/rdf-schema#comment")
                               (s-var "result"))))
    (if description
        (format nil "Retrieves resource described as: ~A" description)
        (format nil "Retrieves resource of type ~A" (string-type-name resource)))))

(defun resource-parameters-for-show (resource)
  "Shows the parameters for showing a single resource."
  (declare (ignore resource))
  (list (jsown:new-js ("name" "id")
                      ("in" "path")
                      ("description" "uuid of the requested resource")
                      ("required" t)
                      ("type" "string"))))

(defun resource-tags-for-show (resource)
  "Returns a set of tags which could be used to group this resource"
  (list "non-relationship" "show" (string-type-name resource)))

(defun resource-responses-for-show (resource)
  "Returns the possible responses for the listing of a resource"
  (jsown:new-js
    ("200" (jsown:new-js
             ("description" (format nil "Instance of ~A" (string-type-name resource)))
             ("schema" ;; TODO: this is not correct
              (jsown:new-js ("$ref" (format nil "#/definitions/~A"
                                            (string-type-name resource)))))))
    ("default" (jsown:new-js
                 ;; we should abide jsonapi, but for real errors we don't
                 ("description" "An error occurred")))))

(defun resource-label-for-create (resource)
  "Returns the label for resource creation."
  (let ((label (triple-pattern-query (ld-class resource)
                                     (s-url "http://www.w3.org/2000/01/rdf-schema#label")
                                     (s-var "result"))))
    (if label
        (format nil "Creates new instance of: ~A" label)
        (format nil "Creates new instance of ~A" (string-type-name resource)))))

(defun resource-description-for-create (resource)
  (let ((description
         (triple-pattern-query (ld-class resource)
                               (s-url "http://www.w3.org/2000/01/rdf-schema#comment")
                               (s-var "result"))))
    (if description
        (format nil "Creates resource of following type: ~A" description)
        (format nil "Creates resource of type ~A" (string-type-name resource)))))

(defun ref-to-resource (resource)
  "Yields the content of the $ref json object for <resource>."
  (format nil "#/definitions/~A" (string-type-name resource)))

(defun single-resource-schema (resource)
  "Yields the schema for referring to a single resource"
  (jsown:new-js ("$ref" (ref-to-resource resource))))

(defun multi-resource-schema (resource)
  "Yields the schema for referring to an array of resources of a particular type."
  (jsown:new-js ("type" "array")
                ("items"
                 (jsown:new-js ("$ref" (ref-to-resource resource))))))

(defun resource-parameters-for-create (resource)
  "Shows the parameters for the listing of a resource."
  (list (jsown:new-js
          ("name" (mu-cl-resources::json-type resource))
          ("in" "body")
          ("description" (format nil "Description of ~A to add"
                                 (mu-cl-resources::json-type resource)))
          ("required" t)
          ("schema" (single-resource-schema resource)))))

(defun resource-tags-for-create (resource)
  "Returns a set of tags which could be used to group this resource"
  (list "non-relationship" "creation" (string-type-name resource)))

(defun resource-responses-for-create (resource)
  "Returns the possible responses for the listing of a resource"
  (jsown:new-js
    ("204" (jsown:new-js
             ("description" (format nil "Creation of ~A instance" (string-type-name resource)))
             ("schema" ;; TODO: is this correct?
              (single-resource-schema resource))))
    ("default" (jsown:new-js
                 ;; we should abide jsonapi, but for real errors we don't
                 ("description" "An error occurred")))))



(defun string-type-name (resource)
  "Retrieves the name of the resource if it were to be represented in a string."
  (string-downcase (symbol-name (mu-cl-resources::resource-name resource))))

(defun resource-label-for-listing (resource)
  "Retrieves the label for the listing of a resource."
  (let ((label (triple-pattern-query (ld-class resource)
                                     (s-url "http://www.w3.org/2000/01/rdf-schema#label")
                                     (s-var "result"))))
    (if label
        (format nil "Lists instances: ~A" label)
        (format nil "Lists instances of ~A" (string-type-name resource)))))

(defun computed-resource-description (resource)
  (let ((description
         (triple-pattern-query (ld-class resource)
                               (s-url "http://www.w3.org/2000/01/rdf-schema#comment")
                               (s-var "result"))))
    (if description
        description
        (format nil "Resource of type ~A" (string-type-name resource)))))

(defun resource-description-for-listing (resource)
  "Retrieves the description for the listing of a resource."
  (let ((description (triple-pattern-query (ld-class resource)
                                           (s-url "http://www.w3.org/2000/01/rdf-schema#comment")
                                           (s-var "result"))))
    (if description
        (format nil "Provides the listing of resources of type ~A, described as: ~A"
                (string-type-name resource) description)
        (format nil "Provides the listing of resources of type ~A"
                (string-type-name resource)))))

(defun resource-parameters-for-listing (resource)
  "Shows the parameters for the listing of a resource."
  (declare (ignore resource))
  (list))

(defun resource-tags-for-listing (resource)
  "Returns a set of tags which could be used to group this resource"
  (list "non-relationship" "listing" (string-type-name resource)))

(defun resource-responses-for-listing (resource)
  "Returns the possible responses for the listing of a resource"
  (jsown:new-js
    ("200" (jsown:new-js
             ("description" (format nil "Listing of ~A instances" (string-type-name resource)))
             ("schema"
              (jsown:new-js ("type" "array")
                            ("items"
                             (jsown:new-js ("$ref" (format nil "#/definitions/~A"
                                                           (string-type-name resource)))))))))
    ("default" (jsown:new-js
                 ;; we should abide jsonapi, but for real errors we don't
                 ("description" "An error occurred")))))

(defun generate-single-resource-definition (resource)
  "Retrieves the definition of the singular form of a particular resource."
  (jsown:new-js
    ((string-type-name resource)
     (jsown:new-js 
       ("type" "object")
       ("description" (computed-resource-description resource))
       ("properties"
        (jsown:new-js
          ("type" (jsown:new-js
                    ("type" "string")
                    ("description" (format nil "Type of this resource, always ~A"
                                           (mu-cl-resources::request-path resource)))))
          ("id" (jsown:new-js
                  ("type" "string")
                  ("description" "uuid of the supplied resource")))
          ("relationships" (jsown:new-js
                             ("type" "object")
                             ("properties" (merge-jsown-objects
                                            (mapcar #'single-resource-relationship-description
                                                    (mu-cl-resources::all-links resource))))))
          ("attributes" (jsown:new-js
                          ("type" "object")
                          ("properties"
                           (let ((properties (jsown:empty-object)))
                             (loop for prop in (mu-cl-resources::ld-properties resource)
                                do (setf (jsown:val properties (mu-cl-resources::json-key prop))
                                         (single-resource-property-description prop)))
                             properties))))))))))

(defun single-resource-relationship-description (relationship)
  (jsown:new-js
    ((mu-cl-resources::json-property-name relationship)
     (jsown:new-js
       ("type" "object")
       ("description" (relationship-description relationship))
       ("properties"
        (jsown:new-js
          ("links"
           (jsown:new-js
             ("type" "object")
             ("description" (format nil "Links to relationships methods for ~A"
                                    (mu-cl-resources::json-property-name relationship)))
             ("properties"
              (jsown:new-js
                ("self" (jsown:new-js ("type" "string")
                                      ("description" "a link for the relationship itself (a \"relationship link\"). This link allows the client to directly manipulate the relationship.")))
                ("related" (jsown:new-js ("type" "string")
                                         ("description" "A \"related resource link\" provides access to resource objects linked in a relationship. When fetched, the related resource object(s) are returned as the response’s primary data.")))))))))))))

(defun relationship-description (relationship)
  (let ((db-description (triple-pattern-query (mu-cl-resources::ld-link relationship)
                                              (s-url "http://www.w3.org/2000/01/rdf-schema#label")
                                              (s-var "result"))))
    (if db-description
        db-description
        (format nil "Provides relation ~A containing objects of type ~A"
                (mu-cl-resources::json-property-name relationship)
                (mu-cl-resources::json-type (mu-cl-resources::referred-resource relationship))))))

(defun property-content-type (property)
  "Retrieves the type of a property for OpenAPI to use."
  (case (mu-cl-resources::resource-type property)
    (:url (jsown:new-js ("type" "string")))
    (:string (jsown:new-js ("type" "string")))
    (:datetime (jsown:new-js ("type" "dateTime")))
    (:date (jsown:new-js ("type" "date")))
    (:g-year (jsown:new-js ("type" "string")))
    (:geometry (jsown:new-js ("type" "string")))
    (:string-set (jsown:new-js ("type" "array")
                               ("items" (jsown:new-js ("type" "string")))))
    (:uri-set (jsown:new-js ("type" "array")
                            ("items" (jsown:new-js ("type" "string")))))
    (:language-string-set (jsown:new-js ("type" "array") ;; this is correct
                                        ("items" (jsown:new-js ("type" "string")))))
    (otherwise "string")))

(defun single-resource-property-description (property)
  "Retrieves the definition for a property of a singular resource"
  (let ((description (let ((database-description
                            (triple-pattern-query (car (last (mu-cl-resources::ld-property-list property)))
                                                  (s-url "http://www.w3.org/2000/01/rdf-schema#label")
                                                  (s-var "result"))))
                       (if database-description
                           database-description
                           (format nil "Retrieves the ~A property of mu-cl-resources type ~A"
                                   (mu-cl-resources::json-property-name property)
                                   (string-downcase (symbol-name
                                                     (mu-cl-resources::resource-type property))))))))
    (merge-jsown-objects (list (jsown:new-js ("description" description))
                               (property-content-type property)))))
              

