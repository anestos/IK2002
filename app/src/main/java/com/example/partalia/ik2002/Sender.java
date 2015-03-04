package com.example.partalia.ik2002;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.Socket;

public class Sender implements Runnable{
    private byte[] msg;
    private String ip;
    private int port;
    private Socket socket;
    private DataOutputStream out;
    private Boolean peer;

    public Sender(byte[] msg, String ip, int port, boolean peer) {
        this.msg = msg;
        this.ip = ip;
        this.port = port;
        this.peer = peer;
    }

    @Override
    public void run() {
        try {
            socket = new Socket(ip, port);
            out = new DataOutputStream(socket.getOutputStream());

            InputStreamReader inputStream = new InputStreamReader(socket.getInputStream());
            BufferedReader input = new BufferedReader(inputStream);

            byte[] request = msg;
            out.writeInt(request.length);
            out.write(request);
            out.flush();
            out.close();
            socket.close();

        }catch (Exception ex){
            ex.printStackTrace();
        }
    }
}