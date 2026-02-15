// This is to run KISS as an electron desktop app


const devMode = false;

const {app, BrowserWindow, globalShortcut, Menu} = require('electron');
const path = require('path');
const { spawn, execSync } = require('child_process');

let mainWindow;
let backendProcess;

function waitForBackend(url, timeout) {
    const http = require('http');
    const start = Date.now();
    return new Promise((resolve, reject) => {
        function tryConnect() {
            if (Date.now() - start > timeout)
                return reject(new Error('Backend did not start within ' + (timeout / 1000) + ' seconds'));
            const req = http.get(url, (res) => {
                resolve();
            });
            req.on('error', () => {
                setTimeout(tryConnect, 500);
            });
            req.setTimeout(2000, () => {
                req.destroy();
                setTimeout(tryConnect, 500);
            });
        }
        tryConnect();
    });
}

function createWindow() {
    if (devMode) {
        mainWindow = new BrowserWindow({
            width: 800,
            height: 600,
            webPreferences: {
                nodeIntegration: true,
                contextIsolation: true
            },
        });

        mainWindow.loadURL('http://localhost:8080');
        mainWindow.webContents.openDevTools();

        globalShortcut.register('CommandOrControl+R', () => {
            mainWindow.webContents.reload();
        });
    } else {
        mainWindow = new BrowserWindow({
            width: 800,
            height: 600,
            webPreferences: {
                nodeIntegration: false,
                contextIsolation: true,
            },
        });

        mainWindow.loadURL('http://localhost:8080');
    }

    mainWindow.on('closed', () => {
        mainWindow = null;
    });

    const menuTemplate = [
        {
            label: 'File',
            submenu: [
                { label: 'Quit', accelerator: 'CommandOrControl+Q', click: () => mainWindow.destroy() }
            ]
        },
        { role: 'editMenu' },
        { role: 'viewMenu' },
        { role: 'help' }
    ];
    Menu.setApplicationMenu(Menu.buildFromTemplate(menuTemplate));
}

app.on('ready', () => {
    // Start the Java backend
    backendProcess = spawn('./bld', ['start-backend']);

    backendProcess.stdout.on('data', (data) => {
        console.log(`Backend: ${data}`);
    });

    backendProcess.stderr.on('data', (data) => {
        console.error(`Backend Error: ${data}`);
    });

    backendProcess.on('close', (code) => {
        console.log(`Backend process exited with code ${code}`);
    });

    // Wait for the backend to be ready before opening the window
    waitForBackend('http://localhost:8080', 60000)
        .then(() => {
            createWindow();
        })
        .catch((err) => {
            console.error('Failed to start backend:', err.message);
            app.quit();
        });
});

app.on('window-all-closed', () => {
    try {
        execSync('./bld stop-backend');
    } catch (e) {
        // ignore errors during shutdown
    }
    app.quit();
});

