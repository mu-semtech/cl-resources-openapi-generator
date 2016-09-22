# OpenAPI resources generator

Generates OpenAPI spec from mu-cl-resources

## Example usage

```
    # Start a triplestore, unless you already have a triplestore in a docker available
    docker run --name my-database -p 8890:8890 tenforce/virtuoso:1.0.0-virtuoso7.2.4
    
    # Generate the sources
    docker run --link my-database:database \
           -v `pwd`/config/resources:/config \
           -v /tmp/:/config/output \
           semtech/mu-cl-resources-openapi-generator
    
    # Copy from the terminal, or open the generated json file
    cat /tmp/openapi.json
```

The last line of output contains the generate calls.  `/tmp/openapi.json` will contain the generated api as well.  You can paste these contents into [the swagger editor](http://editor.swagger.io/#/) to get a preview.  Use the JSON import of the File menu.
