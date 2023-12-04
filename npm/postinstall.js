const fs = require('fs');
const os = require('os');
const {getBinaryUrl, downloadAndUnzip, getBinaryFileName} = require('./binary-manager');

async function setupBinary() {
    try {
        if (!fs.existsSync('./cli')) {
            fs.mkdirSync('./cli');
        }
        const binaryPath = getBinaryUrl();
        await downloadAndUnzip(binaryPath);
    } catch (error) {
        throw new Error(`Failed to download the binary: ${error.message}`);
    }
}

async function setBinaryPermissions() {
    try {
        if (os.platform() === 'win32') {
            return;
        }
        const binaryFileName = getBinaryFileName();
        await fs.promises.chmod(binaryFileName, 0o755);
    } catch (error) {
        throw new Error(`Failed to set permissions on the binary: ${error.message}`);
    }
}

async function main() {
    try {
        await setupBinary();
        await setBinaryPermissions();
    } catch (error) {
        throw new Error(`Error in post-install process: ${error.message}`);
    }
}

main();