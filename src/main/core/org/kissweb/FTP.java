/*
 * Copyright 2022 STACK360 LLC
 * All rights reserved.
 */

package org.kissweb;

import java.io.*;
import java.net.*;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * Transfer files to or from a remote FTP server.
 * 
 * @author Blake McBride
 */
public class FTP implements AutoCloseable {
    
    private static final boolean showInteraction = false;
    
    private Socket socket;
    private BufferedReader reader;
    private BufferedWriter writer;
    
    /**
     * Form a new FTP connection.  This actually logs into the system.
     * 
     * @param url
     * @param user
     * @param pw
     * @throws IOException
     * @throws Exception 
     */
    public FTP(String url, String user, String pw) throws IOException, Exception {
        this(url, 21, user, pw);
    }
    
    /**
     * Form a new FTP connection.  This actually logs into the system.
     * 
     * @param url
     * @param port
     * @param user
     * @param pw
     * @throws IOException
     * @throws Exception 
     */    
    public FTP(String url, int port, String user, String pw) throws IOException, Exception {
        try {
            socket = new Socket(url, port);
            reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
            
            check("220 ");
               
            cmd("USER " + user, "331 ");
            cmd("PASS " + pw, "230 ");
            cmd("TYPE I", "200 ");   // binary mode
        } catch (Exception ex) {
            if (writer != null)
                try {
                    writer.close();
                } catch (IOException ex1) {
                    //  ignore
                }
            if (reader != null)
                try {
                    reader.close();
                } catch (IOException ex1) {
                    //  ignore
                }
            if (socket != null)
                try {
                    socket.close();
                } catch (IOException ex1) {
                    //  ignore
                }
            throw ex;
        }
    }

    private ArrayList<String> getResponse(int maxTimeoutSec) throws IOException, InterruptedException {
        ArrayList<String> allLines = new ArrayList<>();
        int elapsedTime = 0;
        while (true) {
            while (!reader.ready()) {
                if (elapsedTime >= maxTimeoutSec)
                    return allLines;
                Thread.sleep(1000);
                elapsedTime++;
            }
            String response = reader.readLine();
            if (showInteraction)
                System.out.println(response);
            allLines.add(response);
            maxTimeoutSec = 1;
            elapsedTime = 0;
        }
    }
    
    /**
     * Check the result of a command and make sure it starts with either code or code2.
     * If not, throw an exception.
     * 
     */
    private ArrayList<String> check(ArrayList<String> allLines, String code, String code2) throws Exception {
        /* I've seen the system return a success code before I get the file.  This is because it sent it, I just didn't receive it yet.
           It is telling me it sent it.  So, I must check the previous response code too.
        */
        for (String response : allLines) {
            if (code == null || response != null && response.startsWith(code) || code2 != null && response != null && response.startsWith(code2))
                return allLines;
        }        
        allLines.addAll(getResponse(30));
        for (String response : allLines) {
            if (code == null || response != null && response.startsWith(code) || code2 != null && response != null && response.startsWith(code2))
                return allLines;
        }
        throw new Exception("Unexpected reply from FTP server");
    }
 
	private ArrayList<String> check(String code) throws Exception {
		return check(new ArrayList<>(), code, null);
	}
 
	private ArrayList<String> check(String code, String code2) throws Exception {
		return check(new ArrayList<>(), code, code2);
    }
 
	private ArrayList<String> check(ArrayList<String> rtn, String code) throws Exception {
		return check(rtn, code, null);
	}
	
    /**
     * Run cmd and expect a response code of <code>code</code>
     * 
     * @param cmd
     * @param code
     * @throws Exception 
     */
    private void cmd(String cmd, String code) throws Exception {
        if (showInteraction)
            System.out.println(cmd);
        writer.write(cmd + "\r\n");
        writer.flush();
        if (code != null)
            check(code);
    }
    
    private void cmd(String cmdStr) throws Exception {
        cmd(cmdStr, null);
    }
    
    /**
     * Change directory on remote system.
     * 
     * @param dir
     * @throws Exception 
     */
    public void cd(String dir) throws Exception {
        cmd("cd " + dir, "250 ");
    }

    /**
     * Transfer a local file to the remote system.
     * 
     * @param localFileName
     * @param remoteFileName
     * @throws Exception 
     */
    public void put(String localFileName, String remoteFileName) throws Exception {
        String ip2;
        int port2;
        cmd("PASV");
        ArrayList<String> lines = getResponse(15);
        if (lines.size() != 1)
            throw new IOException("Unexpected FTP response");
        String response = lines.get(0);
        int oparen = response.indexOf('(');
        int cparen = response.indexOf(')', oparen + 1);
        if (cparen > 0) {
            StringTokenizer tokenizer = new StringTokenizer(response.substring(oparen + 1, cparen), ",");
            ip2 = tokenizer.nextToken() + "." + tokenizer.nextToken() + "." + tokenizer.nextToken() + "." + tokenizer.nextToken();
            port2 = Integer.parseInt(tokenizer.nextToken()) * 256 + Integer.parseInt(tokenizer.nextToken());
        } else
            throw new Exception("FTP: invalid response to PASV");

        cmd("STOR " + remoteFileName, null);

        final Socket dataSocket = new Socket();
        final SocketAddress addr = new InetSocketAddress(ip2, port2);
        dataSocket.connect(addr, 1000000000);

        check("150 ", "125 ");

        final BufferedInputStream bis = new BufferedInputStream(new FileInputStream(new File(localFileName)));

        final BufferedOutputStream bos = new BufferedOutputStream(dataSocket.getOutputStream());
        final byte[] buffer = new byte[1024];
        int bytesRead;
        while ((bytesRead = bis.read(buffer)) != -1)
            bos.write(buffer, 0, bytesRead);
        bos.flush();
        bos.close();
        bis.close();
        dataSocket.close();

        check("226");
    }
    
    /**
     * Transfer a file to a remote system.
     * 
     * @param fileName same file name on both systems
     * @throws Exception 
     */
    public void put(String fileName) throws Exception {
        put(fileName, fileName);
    }

    /**
     * Transfer a file from the remote server to the local machine.
     * 
     * @param remoteFileName
     * @param localFileName
     * @throws Exception 
     */
    public void get(String remoteFileName, String localFileName) throws Exception {
        String ip2;
        int port2;
        cmd("PASV");
        ArrayList<String> lines = getResponse(15);
        if (lines.size() != 1)
            throw new IOException("Unexpected FTP response");
        String response = lines.get(0);
        int oparen = response.indexOf('(');
        int cparen = response.indexOf(')', oparen + 1);
        if (cparen > 0) {
            StringTokenizer tokenizer = new StringTokenizer(response.substring(oparen + 1, cparen), ",");
            ip2 = tokenizer.nextToken() + "." + tokenizer.nextToken() + "." + tokenizer.nextToken() + "." + tokenizer.nextToken();
            port2 = Integer.parseInt(tokenizer.nextToken()) * 256 + Integer.parseInt(tokenizer.nextToken());
        } else
            throw new Exception("FTP: invalid response to PASV");

        cmd("RETR " + remoteFileName);

        final Socket dataSocket = new Socket();
        final SocketAddress addr = new InetSocketAddress(ip2, port2);
        dataSocket.connect(addr, 1000000000);

        ArrayList<String> rtn = check("150 ", "125 ");
        
//        if (true) return;

        final BufferedInputStream bis = new BufferedInputStream(dataSocket.getInputStream());

        final BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(new File(localFileName)));
        final byte[] buffer = new byte[1024];
        int bytesRead;
        while ((bytesRead = bis.read(buffer)) != -1)
            bos.write(buffer, 0, bytesRead);
        bos.flush();
        bos.close();
        bis.close();
        dataSocket.close();

        check(rtn, "226");
    }
        
    /**
     * Close the connection.
     * 
     * @throws Exception 
     */
    @Override
    public void close() throws Exception {
        if (writer != null)
            cmd("QUIT");
        if (writer != null)
            try {
                writer.close();
            } catch (IOException ex1) {
                //  ignore
            }
        if (reader != null)
            try {
                reader.close();
            } catch (IOException ex1) {
                //  ignore
            }
        if (socket != null)
            try {
                socket.close();
            } catch (IOException ex1) {
                //  ignore
            }
    }

    public static void main(String args[]) throws Exception {
        FTP ftp;

        ftp = new FTP("test.rebex.net", "demo", "password");
        ftp.get("readme.txt", "abc.txt");
        ftp.get("readme.txt", "def.txt");
        ftp.close();
    }
}
