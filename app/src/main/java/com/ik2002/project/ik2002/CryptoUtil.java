package com.ik2002.project.ik2002;

import android.util.Base64;

import org.bouncycastle.util.Arrays;

import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.spec.InvalidParameterSpecException;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;


public class CryptoUtil {

    public static String encrypt(String message, String sessionKey) {
        Cipher cipher;

        byte[] decodedKey = Base64.decode(sessionKey.getBytes(), Base64.DEFAULT);
        SecretKey keyTmp = new SecretKeySpec(decodedKey, 0, decodedKey.length, "PBKDF2WithHmacSHA1");
        Key myKey = new SecretKeySpec(keyTmp.getEncoded(), "AES");
        byte[] iv = null;
        byte[] output = null;
        try {

            cipher = Cipher.getInstance("AES/CTR/NoPadding", "BC");
            cipher.init(Cipher.ENCRYPT_MODE, myKey);
            AlgorithmParameters params = cipher.getParameters();
            iv = params.getParameterSpec(IvParameterSpec.class).getIV();
            output = cipher.doFinal(message.getBytes());

        } catch (NoSuchAlgorithmException | NoSuchProviderException | NoSuchPaddingException | IllegalBlockSizeException | BadPaddingException | InvalidParameterSpecException | InvalidKeyException e) {
            e.printStackTrace();
        }

        byte[] everything = Arrays.concatenate(iv, output);

        return Base64.encodeToString(everything, Base64.DEFAULT);

    }

    public static String decrypt(String buffer, String sessionKey) {

        byte[] bufferDec = org.bouncycastle.util.encoders.Base64.decode(buffer);
        byte[] decodedKey = org.bouncycastle.util.encoders.Base64.decode(sessionKey.getBytes());

        SecretKey keyTmp = new SecretKeySpec(decodedKey, 0, decodedKey.length, "PBKDF2WithHmacSHA1");
        Key myKey = new SecretKeySpec(keyTmp.getEncoded(), "AES");

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

        return "Cannot Decrypt message";
    }

    public static String decryptWithIv(String buffer, String sessionKey, byte[] iv) {

        byte[] bufferDec = org.bouncycastle.util.encoders.Base64.decode(buffer);
        byte[] decodedKey = org.bouncycastle.util.encoders.Base64.decode(sessionKey.getBytes());
        byte[] decodedIv = org.bouncycastle.util.encoders.Base64.decode(iv);

        SecretKey keyTmp = new SecretKeySpec(decodedKey, 0, decodedKey.length, "PBKDF2WithHmacSHA1");
        Key myKey = new SecretKeySpec(keyTmp.getEncoded(), "AES");

        Cipher cipher;
        try {
            cipher = Cipher.getInstance("AES/CTR/NoPadding", "BC");
            cipher.init(Cipher.DECRYPT_MODE, myKey, new IvParameterSpec(decodedIv));
            byte[] decrypted = cipher.doFinal(bufferDec);

            return new String(decrypted);


        } catch (NoSuchAlgorithmException | NoSuchProviderException | NoSuchPaddingException | InvalidAlgorithmParameterException | InvalidKeyException | BadPaddingException | IllegalBlockSizeException e) {
            e.printStackTrace();
        }

        return "Cannot Decrypt message";
    }

    public static String create_nonce() {
        byte[] random = new byte[8];
        SecureRandom rd = new SecureRandom();
        rd.nextBytes(random);
        return new String(org.bouncycastle.util.encoders.Base64.encode(random));

    }
}
