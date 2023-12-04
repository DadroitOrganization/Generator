const os = require('os');
const fs = require('fs');
const JSZip = require('jszip');
const https = require('https');
const path = require('path');

const BASE_URL = 'https://github.com/DadroitOrganization/Generator/releases/download/Release_Version_1.0.0.351';

function getBinaryFileName() {
    switch (os.platform()) {
        case 'darwin':
        case 'linux':
            return path.join(__dirname, 'cli', 'JSONGeneratorCLI');
        case 'win32':
            return path.join(__dirname, 'cli', 'JSONGeneratorCLI.exe');
        default:
            throw new Error('Unsupported platform for getBinaryFileName');
    }
}

function getBinaryUrl() {
    switch (os.platform()) {
        case 'darwin':
            return `${BASE_URL}/JSONGeneratorCLI-macOs-x86_64.zip`;
        case 'win32':
            return `${BASE_URL}/JSONGeneratorCLI-Windows-x86_64.zip`;
        case 'linux':
            return `${BASE_URL}/JSONGeneratorCLI-linux-x86_64.zip`;
        default:
            throw new Error('Unsupported platform for getBinaryUrl');
    }
}

async function extractBinary() {
    const zipFilePath = './cli/JSONGeneratorCLI.zip';
    const extractPath = './cli';
    const zipFileData = fs.readFileSync(zipFilePath);
    const zip = await JSZip.loadAsync(zipFileData);

    // Assuming there's only one file in the zip
    const fileName = Object.keys(zip.files)[0];
    const file = zip.files[fileName];
    const destPath = path.join(extractPath, fileName);

    if (file.dir) {
        fs.mkdirSync(destPath, {recursive: true});
    } else {
        const content = await file.async('nodebuffer');
        fs.writeFileSync(destPath, content);
    }
    //Delete zip after extraction
    fs.unlinkSync(zipFilePath);
}

function downloadAndUnzip(url) {
    return new Promise(async (resolve, reject) => {
        const downloadHelper = async (currentUrl) => {
            https.get(currentUrl, (response) => {
                if (response.statusCode === 302 || response.statusCode === 301) {
                    // Redirection handling
                    const newUrl = response.headers.location;
                    if (typeof newUrl === 'string') {
                        downloadHelper(newUrl);
                    } else {
                        reject(new Error('Location header is missing in the HTTP redirect response'));
                    }
                    return;
                }
                if (response.statusCode !== 200) {
                    reject(new Error(`Failed to download CLI. HTTP Status Code: ${response.statusCode}`));
                    return;
                }
                response.pipe(fs.createWriteStream('./cli/JSONGeneratorCLI.zip'))
                    .on('close', async () => {
                        try {
                            await extractBinary();
                            resolve();
                        } catch (error) {
                            reject(new Error(`Error during extraction: ${error.message}`));
                        }
                    })
                    .on('error', (error) => reject(new Error(`Error during file writing: ${error.message}`)));
            }).on('error', (error) => reject(new Error(`Error during HTTPS request: ${error.message}`)));
        };
        await downloadHelper(url);
    });
}

module.exports = {getBinaryUrl, getBinaryFileName, downloadAndUnzip};