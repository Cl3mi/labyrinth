const fs = require('node:fs');
const path = require('node:path');
const { execSync } = require('node:child_process');
const process = require('node:process');

const args = process.argv.slice(2);
const tokenArg = args[0];
const countArg = args[1];

const CONFIG = {
    LOCAL_JAR: path.join(__dirname, 'target', 'labyrinth-server-1.0.0.jar'),
    DEPLOY_URL: 'https://lab.suda.sh/deploy',
    SECRET_TOKEN: tokenArg || 'group1serverpw',
    COUNT: countArg || '1'
};

async function main() {
    console.log(`Using Deployment Token: ${CONFIG.SECRET_TOKEN.substring(0, 3)}... (masked)`);
    console.log(`Target Instance Count: ${CONFIG.COUNT}`);

    try {
        console.log("Building JAR with Maven...");
        const isWindows = process.platform === 'win32';
        const mavenCmd = isWindows ? 'mvnw.cmd' : './mvnw';

        try {
            execSync(`${mavenCmd} clean package`, { stdio: 'inherit' });
        } catch (err) {
            console.error("Maven build failed.");
            process.exit(1);
        }

        console.log("Uploading JAR to Deployer...");

        if (!fs.existsSync(CONFIG.LOCAL_JAR)) {
            console.error(`JAR file not found at: ${CONFIG.LOCAL_JAR}`);
            process.exit(1);
        }

        const fileBuffer = fs.readFileSync(CONFIG.LOCAL_JAR);
        const formData = new FormData();

        const blob = new Blob([fileBuffer], { type: 'application/java-archive' });
        formData.append('file', blob, 'app.jar');

        const deployUrlWithParams = `${CONFIG.DEPLOY_URL}?count=${CONFIG.COUNT}`;

        const response = await fetch(deployUrlWithParams, {
            method: 'POST',
            headers: {
                'x-deploy-token': CONFIG.SECRET_TOKEN,
                'x-deployment-count': CONFIG.COUNT
            },
            body: formData
        });

        const resultText = await response.text();

        if (response.ok) {
            console.log("Server Response:");
            console.log(resultText);
        } else {
            console.error(`Upload Failed [${response.status}]: ${resultText}`);
        }

    } catch (error) {
        console.error("Unexpected Error during deployment:", error);
    }
}

main();