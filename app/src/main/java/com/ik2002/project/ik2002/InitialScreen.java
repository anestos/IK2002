package com.ik2002.project.ik2002;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.util.Base64;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;


import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.security.AlgorithmParameters;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.spec.InvalidParameterSpecException;
import java.util.Arrays;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;


public class InitialScreen extends Activity {
    private EditText txtPeerName;
    private EditText message;
    private String name;
    private Key key;
    private String serverIp;
    private ExecutorService ex;
    ServerReceiver socketServerThread;
    private String stringedKey;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_initial_screen);
        Button btnContact = (Button) findViewById(R.id.btn_contact);
        txtPeerName = (EditText) findViewById(R.id.peer_name);
        message = (EditText) findViewById(R.id.message);

        SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
        name = sharedPref.getString("user_name", "nada");
        serverIp = sharedPref.getString("serverIp", "");
        stringedKey = sharedPref.getString("user_key", "key");


        if (stringedKey.equals("key") || name.equals("nada")) {
            Intent intent = new Intent(InitialScreen.this, MainActivity.class);
            startActivity(intent);
        } else {
            byte[] decodedKey = Base64.decode(stringedKey.getBytes(), Base64.DEFAULT);
            SecretKey keyTmp = new SecretKeySpec(decodedKey, 0, decodedKey.length, "PBKDF2WithHmacSHA1");
            key = new SecretKeySpec(keyTmp.getEncoded(), "AES");


            ex = Executors.newFixedThreadPool(2);
            socketServerThread = new ServerReceiver();
            ex.submit(socketServerThread);
        }

        btnContact.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {

                if (txtPeerName.getText().toString().trim().length() > 0 && message.getText().toString().length() > 0) {




                    String myName = name;
                    String peerName = txtPeerName.getText().toString();

                    Toast.makeText(getApplicationContext(), "Please wait, contacting " + peerName, Toast.LENGTH_LONG).show();

                    byte[] toSend;
                    byte[] random = new byte[8];
                    SecureRandom rd = new SecureRandom();
                    rd.nextBytes(random);

                    byte[] rndEnc = org.bouncycastle.util.encoders.Base64.encode(random);
                    String toEncrypt = new String(rndEnc) + myName + "|" + peerName;

                    try {

                        Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding", "BC");
                        cipher.init(Cipher.ENCRYPT_MODE, key);
                        AlgorithmParameters params = cipher.getParameters();
                        byte[] iv = params.getParameterSpec(IvParameterSpec.class).getIV();

                        byte[] output = cipher.doFinal(toEncrypt.getBytes());

                        toSend = new byte[iv.length + output.length];
                        System.arraycopy(iv, 0, toSend, 0, iv.length);
                        System.arraycopy(output, 0, toSend, iv.length, output.length);

                        byte[] encoded = org.bouncycastle.util.encoders.Base64.encode(toSend);

                        Callable<String> callable = new Sender(encoded, serverIp, 8080);
                        Future<String> send = ex.submit(callable);

                        KdcReply kdcReply = new KdcReply(send, key);

                        if (new String(rndEnc).equals(kdcReply.getNonce()) && peerName.equals(kdcReply.getPeerName())) {

                            // handshake (mutual authentication) with peer
                            Callable<Boolean> callableAuthenticator = new Authenticator(kdcReply.getPeerIp(), 6666, kdcReply.getTicket(), kdcReply.getSessionKey());
                            Future<Boolean> authenticated = ex.submit(callableAuthenticator);


                            try {

                                if (authenticated.get()) {
                                    SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
                                    SharedPreferences.Editor editor = sharedPref.edit();
                                    editor.putString("sessionKey", kdcReply.getSessionKey());
                                    editor.putString("peerIp", kdcReply.getPeerIp());
                                    editor.putString("peerName", peerName);
                                    editor.putString("initialMessage", message.getText().toString());
                                    editor.commit();
                                    Killer.getInstance().setRunning(false);

                                    Intent intent = new Intent(InitialScreen.this, ChatActivity.class);
                                    startActivity(intent);
                                } else {
                                    Toast.makeText(getApplicationContext(),
                                            "Cannot connect to " + kdcReply.getPeerName(), Toast.LENGTH_LONG).show();


                                }


                            } catch (InterruptedException | ExecutionException e) {
                                e.printStackTrace();
                            }

                        }

                    } catch (NoSuchAlgorithmException | NoSuchProviderException | NoSuchPaddingException | InvalidKeyException | InvalidParameterSpecException | BadPaddingException | IllegalBlockSizeException e) {
                        e.printStackTrace();
                    }


                } else {
                    Toast.makeText(getApplicationContext(),
                            "Please fill the appropriate fields", Toast.LENGTH_LONG).show();
                }
            }

        });


    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_initial_screen, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_delete_key) {
            Killer.getInstance().setRunning(false);
            SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
            SharedPreferences.Editor editor = sharedPref.edit();
            editor.putString("user_name", "nada");
            editor.putString("user_key", "key");
            editor.commit();

            Intent intent = new Intent(InitialScreen.this,
                    MainActivity.class);

            startActivity(intent);
            return true;
        }
        if (id == R.id.action_information) {
            Intent intent = new Intent(InitialScreen.this, Information.class);
            startActivity(intent);
        }

        return super.onOptionsItemSelected(item);
    }

    // Server Code
    private class ServerReceiver implements Runnable {
        private ServerSocket sSocket;
        private Socket cSocket;
        private BufferedReader in;
        private PrintWriter pw;
        private String buffer;
        private String tempKey = "tempKey";
        private String tempName = "tempName";
        private String nonce3;

        public void sendMessage(String s) {
            pw.println(s);
            pw.flush();
        }

        @Override
        public void run() {
            try {
                sSocket = new ServerSocket(6666);
                cSocket = sSocket.accept();
                pw = new PrintWriter(cSocket.getOutputStream(), true);
                while (Killer.getInstance().getRunning()) {
                    in = new BufferedReader(new InputStreamReader(cSocket.getInputStream()));
                    buffer = in.readLine();

                    String toSend = serverHandleIncomingRequest(buffer, cSocket.getInetAddress().getHostAddress());
                    sendMessage(toSend);

                    buffer = in.readLine();

                    if (handleNonce3(buffer)) {

                        sendMessage(CryptoUtil.encrypt("authenticated", tempKey));
                        Killer.getInstance().setRunning(false);

                        SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
                        SharedPreferences.Editor editor = sharedPref.edit();
                        editor.putString("sessionKey", tempKey);
                        editor.putString("peerIp", Arrays.toString(cSocket.getInetAddress().getAddress()));
                        editor.putString("peerName", tempName);
                        editor.commit();

                        Intent intent = new Intent(InitialScreen.this, ChatActivity.class);
                        startActivity(intent);
                    } else {
                        cSocket.close();
                        cSocket = sSocket.accept();
                        pw = new PrintWriter(cSocket.getOutputStream(), true);

                    }
                }
                sSocket.close();

            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }

        private Boolean handleNonce3(String buffer) {
            String decryptedMsg = CryptoUtil.decrypt(buffer, tempKey);
            return nonce3.equals(decryptedMsg);


        }


        private String serverHandleIncomingRequest(String msg, String userIp) {

            // verify ticket decrypt nonce and reply with encrypted(nonce+nonce)
            String[] msgSplit = msg.split("\\|");
            String encryptedTicket = msgSplit[0];
            String encryptedNonce2 = msgSplit[1];

            byte[] ivTicket = Arrays.copyOfRange(encryptedTicket.getBytes(), 0, 24);
            byte[] ticket = Arrays.copyOfRange(encryptedTicket.getBytes(), 24, encryptedTicket.getBytes().length);


            String decryptedTicket = CryptoUtil.decryptWithIv(new String(ticket), stringedKey, ivTicket);
            String[] ticketSplit = decryptedTicket.split("\\|");

            tempKey = ticketSplit[2];
            tempName = ticketSplit[0];

            if (ticketSplit[1].equals(userIp)) {
                String decryptedNonce = CryptoUtil.decrypt(encryptedNonce2, tempKey);
                nonce3 = CryptoUtil.create_nonce();
                return CryptoUtil.encrypt(decryptedNonce + "|" + nonce3, tempKey);
            }
            return "user in not authenticated";
        }

    }


}


