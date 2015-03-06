package com.example.partalia.ik2002;


import java.security.Key;
import java.util.Arrays;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class KdcReply {
    private byte [] nonce;
    private String peerName;
    private String peerIp;
    private String ticket;
    private String sessionKey;
    private byte[] msg;
    private byte[] iv;


    public KdcReply(Future<String> send, Key key) {
        //Todo Decrypt send with key and save values

        try {
            String received = send.get();
            msg = new byte[received.getBytes().length];
            msg = received.getBytes();
            iv = new byte[16];
            iv = Arrays.copyOfRange(msg, 0 , 15);
            System.out.println("Msg:"+ new String(msg));
            System.out.println("IV:"+ new String(iv));


        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExecutionException e) {
            e.printStackTrace();
        }


    }

    public String getPeerName() {
        return peerName;
    }

    public byte[] getNonce() {
        return nonce;
    }

    public String getPeerIp() {
        return peerIp;
    }

    public String getTicket() {
        return ticket;
    }

    public String getSessionKey() {
        return sessionKey;
    }
}
