package com.example.partalia.ik2002;


import java.security.Key;
import java.util.concurrent.Future;

public class KdcReply {
    private byte [] nonce;
    private String peerName;
    private String peerIp;
    private String ticket;
    private String sessionKey;


    public KdcReply(Future<String> send, Key key) {
        //Todo Decrypt send with key and save values


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
