# [Dadroit JSON Generator][DOMAIN]

This tool helps generate JSON data using a custom JSON-based and functional template language. It is fast, capable of generating 100K objects per second, and powerful, with many features such as variants (numbers, strings, and arrays), loops, templates, and math functions (random, min, max, count, etc.). Additionally, it supports including templates within each other and reusing them.

You can use it for JSON-intensive use cases, such as testing applications and APIs. For example, we use it to test our remarkable [Dadroit JSON Viewer](https://dadroit.com/).

If you want to learn more, there is a [detailed blog post](https://dadroit.com/blog/json-generator-how-to-create-dummy-json-data/) about how to put this tool into use. Additionally, there are some samples in the Samples directory that describe the Generator's features in steps.

## Usage
To use it, you need to have a template file like:

```javascript
{
    "Name": "$FirstName",
    Value: {
        X: 1,
        Y: 2
    },
    Books: {
        $Random: ["B1", "B2", "B3"]
    },
    Age: {
        $Random: {
            $Min: 10,
            $Max: 20
        }
    }
}
```
Save it to a file and then execute:

```console
JSONGeneratorCLI Sample.json
```

And you will get a new file containing:

```javascript
{
    "Name": "John",
    "Value": {
        "X": 1,
        "Y": 2
    },
    "Books": "B3",
    "Age": 13
}
```

For more samples, check out the Samples directory.

## Supported Systems
* Windows
* Mac
* Linux

## Third-Party
* [Synopse mORMot framework](https://github.com/synopse/mORMot2)

## Build Instructions
* Use [Lazarus & FPC](https://www.lazarus-ide.org). Open the project and run.

[//]: # "LINKS"
[DOMAIN]: https://dadroit.com/
