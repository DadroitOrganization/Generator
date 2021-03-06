# [Dadroit JSON Generator][DOMAIN]

This tool helps in generating JSON data using a custom JSON based and functional template language.
It is fast (100K objects per second), and it is powerful with many features, including Variants (Numbers, Strings, Arrays), Loop, Template, and math functions like Random, Min, Max, Count, and more. Moreover, it supports including templates into each other and reusing them.
You can use it for JSON intensive use cases like testing applications and APIs. For example, we are using it for testing remarkable [Dadroit JSON Viewer][DOMAIN].

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

There are some samples in the Samples directory, showing the features step by step to learn more.

## Sample data

Some small and big JSON example files are already generated and available. You can download the latest version from the [releases page](https://github.com/DadroitOrganization/Generator/releases).

## Supported Systems
* Windows
* Mac
* Linux

## Third-Party
* [Synopse mORMot framework](https://github.com/synopse/mORMot)

## Build Instructions
* Use [Lazarus & FPC](https://www.lazarus-ide.org). Open the project and run.

[//]: # "LINKS"
[DOMAIN]: https://dadroit.com/
