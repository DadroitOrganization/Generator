#!/usr/bin/env node

const {exec} = require('child_process');
const path = require('path');
const {getBinaryFileName} = require('./binary-manager');

function executeBinary(binaryFileName, filePath) {
    return new Promise((resolve, reject) => {
        exec(`${binaryFileName} "${filePath}"`, (error, stdout, stderr) => {
            if (error) {
                reject(new Error(`Execution error: ${error.message}`));
                return;
            }
            if (stderr) {
                reject(new Error(`Execution stderr: ${stderr}`));
                return;
            }
            resolve(stdout);
        });
    });
}

async function main() {
    if (process.argv.length !== 3) {
        console.error("Please provide 2 arguments: 'json-generator' and a valid file address.");
        return;
    }

    const userProvidedFilePath = process.argv[2];
    const binaryFileName = getBinaryFileName();
    let filePath = path.resolve(userProvidedFilePath);
    await executeBinary(binaryFileName, filePath);
}

main();