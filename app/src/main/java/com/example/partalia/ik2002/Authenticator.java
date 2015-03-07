package com.example.partalia.ik2002;

import android.content.Context;
import android.content.SharedPreferences;
import android.util.Base64;

import org.bouncycastle.util.Arrays;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.Socket;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.spec.InvalidParameterSpecException;
import java.util.concurrent.Callable;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

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
        encryptedNonce2 = encrypt(new String(nonce2String), sessionKey);
    }

    @Override
    public Boolean call() {
        try {
            socket = new Socket(ip, port);
            out = new DataOutputStream(socket.getOutputStream());

            InputStreamReader inputStream = new InputStreamReader(socket.getInputStream());
            BufferedReader input = new BufferedReader(inputStream);


            out.writeUTF(ticket+"|"+encryptedNonce2);
            out.flush();

            BufferedReader fromServer = new BufferedReader( new InputStreamReader(socket.getInputStream()));
            String line = fromServer.readLine();

            String reply = decrypt(line, sessionKey);
            if( reply.contains(nonce2String)) {
                String nonce3String = reply.replace(nonce2String, "");

                String encryptedNonce3 = encrypt(new String(nonce3String), sessionKey);
                out.writeUTF(encryptedNonce3);
                out.flush();

                BufferedReader fromServer2 = new BufferedReader( new InputStreamReader(socket.getInputStream()));
                String line2 = fromServer2.readLine();
                String reply2 = decrypt(line2, sessionKey);

                if (reply2.equals("authenticated")){
                    inputStream.close();
                    out.close();
                    socket.close();
                    return true;
                }
            }

            inputStream.close();
            out.close();
            socket.close();
            return false;

        }catch (Exception ex){
            ex.printStackTrace();
        }
        return false;
    }

    private String encrypt(String message, String sessionKey) {
        Cipher cipher;

        byte[] decodedKey = Base64.decode(sessionKey.getBytes(), Base64.DEFAULT);
        SecretKey keytmp = new SecretKeySpec(decodedKey, 0, decodedKey.length, "PBKDF2WithHmacSHA1");
        Key myKey = new SecretKeySpec(keytmp.getEncoded(), "AES");
        byte[] iv = null;
        byte[] output = null;
        try {

            cipher = Cipher.getInstance("AES/CTR/NoPadding", "BC");
            cipher.init(Cipher.ENCRYPT_MODE, myKey);
            AlgorithmParameters params = cipher.getParameters();
            iv = params.getParameterSpec(IvParameterSpec.class).getIV();
            System.out.println(""+iv.length);
            output = cipher.doFinal(message.getBytes());

        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (NoSuchProviderException e) {
            e.printStackTrace();
        } catch (NoSuchPaddingException e) {
            e.printStackTrace();
        } catch (IllegalBlockSizeException e) {
            e.printStackTrace();
        } catch (BadPaddingException e) {
            e.printStackTrace();
        } catch (InvalidParameterSpecException e) {
            e.printStackTrace();
        } catch (InvalidKeyException e) {
            e.printStackTrace();
        }

        byte[] everything = Arrays.concatenate(iv, output);
        String toSend =  Base64.encodeToString(everything, Base64.DEFAULT);

        return toSend;

    }

    private String decrypt(String buffer, String sessionKey) {

        byte[] bufferDec = org.bouncycastle.util.encoders.Base64.decode(buffer);

        byte[] decodedKey = Base64.decode(sessionKey.getBytes(), Base64.DEFAULT);
        SecretKey keytmp = new SecretKeySpec(decodedKey, 0, decodedKey.length, "PBKDF2WithHmacSHA1");
        Key myKey = new SecretKeySpec(keytmp.getEncoded(), "AES");

        byte[] iv;
        iv = java.util.Arrays.copyOfRange(bufferDec, 0, 16);

        byte[] encrypted;
        encrypted = java.util.Arrays.copyOfRange(bufferDec, 16, bufferDec.length);

        Cipher cipher;
        try {
            cipher = Cipher.getInstance("AES/CTR/NoPadding", "BC");
            cipher.init(Cipher.DECRYPT_MODE, myKey, new IvParameterSpec(iv));
            byte[] decrypted = cipher.doFinal(encrypted);

            return new String(decrypted);


        } catch (NoSuchAlgorithmException | NoSuchProviderException | NoSuchPaddingException | InvalidAlgorithmParameterException | InvalidKeyException | BadPaddingException | IllegalBlockSizeException e) {
            e.printStackTrace();
        }

        return "empty";

    }
}

