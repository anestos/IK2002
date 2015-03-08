package com.example.partalia.ik2002;

import android.content.Context;
import android.content.SharedPreferences;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.Socket;
import java.security.SecureRandom;
import java.util.concurrent.Callable;

public class Authenticator implements Callable<Boolean> {
    private String ip;
    private int port;
    private Socket socket;
    private DataOutputStream out;
    private String sessionKey;
    private String ticket;
    private byte[] nonce2;
    private String encryptedNonce2;
    private String nonce2String;
    private BufferedReader input;


    public Authenticator(String ip, int port, String ticket, String sessionKey) {
        this.ip = ip;
        this.port = port;
        this.ticket = ticket;
        this.sessionKey = sessionKey;

        byte[] random = new byte[8];
        SecureRandom rd = new SecureRandom();
        rd.nextBytes(random);
        nonce2 = org.bouncycastle.util.encoders.Base64.encode(random);
        nonce2String = new String(nonce2);
        encryptedNonce2 = CryptoUtil.encrypt(new String(nonce2String), sessionKey);
    }

    @Override
    public Boolean call() {
        try {
            socket = new Socket(ip, port);
            out = new DataOutputStream(socket.getOutputStream());

            byte[] steiltoToGamidi = (ticket+"|"+encryptedNonce2).getBytes();
            out.write(steiltoToGamidi);
            out.flush();


            InputStreamReader inputStream = new InputStreamReader(socket.getInputStream());
            input = new BufferedReader(inputStream);
            String line = input.readLine();


            String reply = CryptoUtil.decrypt(line, sessionKey);
            String[] splitReply = reply.split("\\|");


            if( splitReply[0].equals(nonce2String)) {

                String nonce3String = splitReply[1];

                String encryptedNonce3 = CryptoUtil.encrypt(nonce3String, sessionKey);
                out.write(encryptedNonce3.getBytes());
                out.flush();

                InputStreamReader inputStream2 = new InputStreamReader(socket.getInputStream());
                BufferedReader input2 = new BufferedReader(inputStream2);

                line = input2.readLine();

                reply = CryptoUtil.decrypt(line, sessionKey);

                if (reply.equals("authenticated")){

                    input.close();
                    input2.close();
                    out.close();
                    socket.close();
                    return true;
                }
            }

            input.close();
            out.close();
            socket.close();
            return false;

        }catch (Exception ex){
            ex.printStackTrace();
        }
        return false;
    }


}

