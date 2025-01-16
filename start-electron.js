// This is to run KISS as an electron desktop app


const devMode = false;

const {app, BrowserWindow, globalShortcut} = require('electron');
const path = require('path');
const { spawn } = require('child_process');

let mainWindow;
let backendProcess;

app.on('ready', () => {
    // Start the Java backend
    backendProcess = spawn('./bld', ['develop-backend']);

    backendProcess.stdout.on('data', (data) => {
        console.log(`Backend: ${data}`);
    });

    backendProcess.stderr.on('data', (data) => {
        console.error(`Backend Error: ${data}`);
    });

    backendProcess.on('close', (code) => {
        console.log(`Backend process exited with code ${code}`);
    });

    if (devMode) {
        mainWindow = new BrowserWindow({
            width: 800,
            height: 600,
            webPreferences: {
                nodeIntegration: true, // Optional, enables Node.js in frontend
                contextIsolation: true // For compatibility, disable if needed
            },
        });

        // Load your frontend file
        //mainWindow.loadFile('src/main/frontend/index.html');
        mainWindow.loadFile(path.join(__dirname, 'src/main/frontend', 'index.html'));

        // Open DevTools for debugging
        mainWindow.webContents.openDevTools();


        // Register a keyboard shortcut for reload
        globalShortcut.register('CommandOrControl+R', () => {
            mainWindow.webContents.reload();
        });

    } else {
        mainWindow = new BrowserWindow({
            width: 800,
            height: 600,
            webPreferences: {
                nodeIntegration: false, // Disable Node.js integration for security
                contextIsolation: true, // Ensure a secure context
            },
        });

        // Load the existing frontend
        mainWindow.loadFile(path.join(__dirname, 'src/main/frontend', 'index.html'));
    }
});

