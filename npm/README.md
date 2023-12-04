# Dadroit JSON Generator NPM Package

![Version](https://img.shields.io/badge/version-1.0.0-brightgreen) [![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](https://github.com/DadroitOrganization/Generator/blob/main/LICENSE.txt)

## Introduction

The Dadroit JSON Generator is a powerful npm package for generating structured JSON test data. Based on the [Dadroit JSON Generator CLI App](https://github.com/DadroitOrganization/Generator), it offers a command-line interface within Node.js applications, making it easy to create mock data for testing and development.

## Installation

To install the package:

```bash
npm install @dadroit/json-generator
```

## Features

- Generate JSON data from custom templates.
- Command-line interface for streamlined data generation.
- Support for complex data structures and custom formats.

## Usage

Use the command line interface to easily generate JSON data from a template.

```bash
json-generator Sample.json
```

## Create Your Custom Templates or Explore Available Ones

This tool generates structured JSON data utilizing customizable template files. You have two main options for the template file preparation:

1. **Use Pre-Made Templates**: Quickly start with a selection of ready-to-use templates available in our [Samples folder](https://github.com/DadroitOrganization/Generator/tree/main/Samples).
2. **Create Your Templates**: Customize your own JSON templates for your specific schema. More details on this are provided in the following sections.

### Advanced Template Customization and Usage

Efficiently create varied and large-scale JSON sample data using the functions provided in the template language.

- **Dynamic Loops**: Use loop syntax for repetitive structures:

  ```json
  {
    "List": {
      "$Loop": {
        "$From": 0,
        "$To": 9,
        "$Var": "I",
        "$Block": {
          "X": {
            "$GetVar": "I"
          }
        }
      }
    }
  }
  ```

- **Random Data Generation**: Incorporate randomness for varied data:

  ```json
  {
    "Age": {
      "$Random": {
        "$Min": 25,
        "$Max": 75
      }
    }
  }
  ```

In this template language, there are more syntaxes to explore, for detailed guidance on creating your custom templates, see our [blog post](https://dadroit.com/blog/json-generator/#dadroit-json-generator-a-tool-for-generating-customizable-mock-data). It offers an in-depth walkthrough of the customization process, providing step-by-step instructions to help you tailor your templates effectively.

## Compatibility

Compatible with Windows, macOS, and Linux. Watch for updates and expanded OS support.

## License

Licensed under [Apache-2.0](https://github.com/DadroitOrganization/JSONGeneratorExtension/blob/main/LICENSE).

## Contributing

Contributions are welcome! Share your templates with the community:

- Check out our [GitHub repository sample folder](https://github.com/DadroitOrganization/Generator/tree/main/Samples) for template examples.
- Submit a pull request with your template, formatted as per the samples.

## Issues and Support

For queries or issues, please [create an issue on GitHub](https://github.com/DadroitOrganization/Generator/issues).