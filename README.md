# [Dadroit JSON Generator](https://dadroit.com/)

Dadroit JSON Generator is a high-performance tool designed for developers in need of rapid and reliable JSON data generation. Utilizing a custom, function-rich template language. It can efficiently generate 100K objects per second, supporting capabilities such as variants, loops, reusable templates, and various math functions like random, min, max, and count.

This tool can be used in various data-related tasks like validating applications and APIs by providing structured JSON sample data, for example, we built it initially to use it for testing our Dadroit JSON Viewer [https://dadroit.com/](https://dadroit.com/).

## **Usage**

### **Quick Start Guide**

Save your template as `sample.json`, for instance, this can be a sample template:

```json
{
    "Name": "$FirstName",
    "Value": {"X": 1,"Y": 2},
    "Books": {"$Random": ["B1", "B2", "B3"]},
    "Age": {"$Random": {"$Min": 10,"$Max": 20}}
}
```

Execute using:

```bash
JSONGeneratorCLI Sample.json
```

Your new file should be like this:

```json
{
    "Name": "John",
    "Value": {"X": 1,"Y": 2},
    "Books": "B3",
    "Age": 13
}
```

Discover additional samples in the [GitHub samples directory](https://github.com/DadroitOrganization/Generator/tree/main/Samples).

For a comprehensive guide on how to use the JSON generator, visit [https://dadroit.com/blog/json-generator/](https://dadroit.com/blog/json-generator/#dadroit-json-generator-a-tool-for-generating-customizable-mock-data).

## **VSCode Extension Available**

We’ve recently published a [VSCode Extension](https://github.com/DadroitOrganization/JSONGeneratorExtension.git), a tool crafted to allow developers to utilize the functionalities of the Dadroit JSON Generator directly within their development environment. Simplify your JSON data generation process without leaving your code editor and get additional insights and usage details in our dedicated [blog post](https://dadroit.com/blog/json-generator-vscode-extension/).

## **Supported Systems**

- Windows
- Mac
- Linux

## **Third-Party**

[Synopse mORMot framework](https://github.com/synopse/mORMot2)

## **Build Instructions**

1. Utilize [Lazarus & FPC](https://www.lazarus-ide.org/)
2. Open the project.
3. Execute the build process.

## **Contributing**

We warmly invite you to contribute to the Dadroit JSON Generator. Whether it’s improving the code, adding templates, or enhancing documentation - every bit helps! 

## **License**

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](https://github.com/DadroitOrganization/Generator/blob/main/LICENSE.txt).

## **Support**

Encounter issues or have suggestions? [Create an issue](https://github.com/DadroitOrganization/Generator/issues).
