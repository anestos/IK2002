package com.ik2002.project.ik2002;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.concurrent.Callable;

public class Sender implements Callable<String> {
    private byte[] msg;
    private String ip;
    private int port;

    public Sender(byte[] msg, String ip, int port) {
        this.msg = msg;
        this.ip = ip;
        this.port = port;
    }

    @Override
    public String call() {
        try {
            Socket socket = new Socket(ip, port);
            DataOutputStream out = new DataOutputStream(socket.getOutputStream());

            InputStreamReader inputStream = new InputStreamReader(socket.getInputStream());

            byte[] request = msg;
            out.writeInt(request.length);
            out.write(request);
            out.flush();

            BufferedReader fromServer = new BufferedReader( new InputStreamReader(socket.getInputStream()));
            String line = fromServer.readLine();

            inputStream.close();
            out.close();
            socket.close();
            return line;

        }catch (Exception ex){
            ex.printStackTrace();
        }
        return "socket problem";
    }
}