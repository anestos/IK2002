package com.example.partalia.ik2002;


import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.util.Arrays;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.IvParameterSpec;

public class KdcReply {
    private String nonce;
    private String peerName;
    private String peerIp;
    private String ticket;
    private String sessionKey;
    private byte[] iv;


    public KdcReply(Future<String> send, Key key) {
        try {
            String received = send.get();
            byte[] msg = received.getBytes();
            byte[] encrypted = Arrays.copyOfRange(msg, 28, received.getBytes().length);

            iv = new byte[16];
            iv = Arrays.copyOfRange(msg, 4 , 28);

            byte[] ivDec = org.bouncycastle.util.encoders.Base64.decode(iv);
            byte[] encryptedDec = org.bouncycastle.util.encoders.Base64.decode(encrypted);

            Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding", "BC");
            cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(ivDec));

            byte[] decrypted = cipher.doFinal(encryptedDec);
            String decryptedString = new String(decrypted);
            String[] decryptedArray = decryptedString.split("\\|");


            this.nonce = decryptedArray[0];
            this.peerName = decryptedArray[1];
            this.peerIp = decryptedArray[2];
            this.sessionKey = decryptedArray[3];
            this.ticket = decryptedArray[4];

        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (ExecutionException e) {
            e.printStackTrace();
        } catch (NoSuchPaddingException e) {
            e.printStackTrace();
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (InvalidKeyException e) {
            e.printStackTrace();
        } catch (NoSuchProviderException e) {
            e.printStackTrace();
        } catch (InvalidAlgorithmParameterException e) {
            e.printStackTrace();
        } catch (BadPaddingException e) {
            e.printStackTrace();
        } catch (IllegalBlockSizeException e) {
            e.printStackTrace();
        }
    }

    public String getPeerName() {
        return peerName;
    }

    public String getNonce() {
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
