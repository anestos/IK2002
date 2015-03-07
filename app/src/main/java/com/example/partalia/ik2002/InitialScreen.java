package com.example.partalia.ik2002;

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
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.security.AlgorithmParameters;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.spec.InvalidParameterSpecException;
import java.util.Arrays;
import java.util.Enumeration;
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
    private Button btnContact;
    private EditText txtPeerName;
    private EditText message;
    private String name;
    private Key key;
    private String serverIp;
    private ExecutorService ex;
    ServerReceiver socketServerThread;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_initial_screen);
        btnContact = (Button) findViewById(R.id.btn_contact);
        txtPeerName = (EditText) findViewById(R.id.peer_name);
        message = (EditText) findViewById(R.id.message);

        SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
        name = sharedPref.getString("user_name", "nada");
        serverIp = sharedPref.getString("serverIp", "");
        String stringedKey = sharedPref.getString("user_key", "key");

        if (stringedKey.equals("key") || name.equals("nada")) {
            Intent intent = new Intent(InitialScreen.this, MainActivity.class);
            startActivity(intent);
        } else {
            byte[] decodedKey = Base64.decode(stringedKey.getBytes(), Base64.DEFAULT);
            SecretKey keytmp = new SecretKeySpec(decodedKey, 0, decodedKey.length, "PBKDF2WithHmacSHA1");
            key = new SecretKeySpec(keytmp.getEncoded(), "AES");
        }

        // server starting
        ex = Executors.newFixedThreadPool(10);
        socketServerThread = new ServerReceiver();
        ex.submit(socketServerThread);


        btnContact.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {

                if (txtPeerName.getText().toString().trim().length() > 0 && message.getText().toString().length() > 0) {

                    // Todo (check) Server stop
                    socketServerThread.killIt();

                    String myName = name;
                    String peerName = txtPeerName.getText().toString();

                    Toast.makeText(getApplicationContext(), "Please wait, contacting " + peerName, Toast.LENGTH_LONG).show();

                    String toEncrypt = "";
                    byte[] toSend;
                    byte[] random = new byte[8];
                    SecureRandom rd = new SecureRandom();
                    rd.nextBytes(random);

                    byte[] rndEnc = org.bouncycastle.util.encoders.Base64.encode(random);
                    System.out.println("Nonce: " + new String(rndEnc));
                    System.out.println("Nonce length:" + rndEnc.length);
                    toEncrypt = new String(rndEnc) + myName + "|" + peerName;

                    try {

                        Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding", "BC");
                        cipher.init(Cipher.ENCRYPT_MODE, key);
                        AlgorithmParameters params = cipher.getParameters();
                        byte[] iv = params.getParameterSpec(IvParameterSpec.class).getIV();

                        byte[] output = cipher.doFinal(toEncrypt.getBytes());

                        toSend = new byte[iv.length + output.length];
                        System.arraycopy(iv, 0, toSend, 0, iv.length);
                        System.arraycopy(output, 0, toSend, iv.length, output.length);

                        byte[] keyEnc = org.bouncycastle.util.encoders.Base64.encode(key.getEncoded());
                        System.out.println("Key: " + new String(keyEnc));

                        byte[] encoded = org.bouncycastle.util.encoders.Base64.encode(toSend);
                        System.out.println("Encoded: " + Arrays.toString(encoded));

                        ExecutorService executor = Executors.newFixedThreadPool(5);
                        Callable<String> callable = new Sender(encoded, serverIp, 8080);
                        Future<String> send = executor.submit(callable);

                        KdcReply kdcReply = new KdcReply(send, key);

                        if (new String(rndEnc).equals(kdcReply.getNonce()) && peerName.equals(kdcReply.getPeerName())) {

                            // handshake (mutual authentication) with peer

                            Callable<Boolean> callableAuthenticator = new Authenticator(kdcReply.getPeerIp(), 9000, kdcReply.getTicket(), kdcReply.getSessionKey());
                            Future<Boolean> authenticated = executor.submit(callableAuthenticator);


                            try {
                                if (authenticated.get()) {
                                    SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
                                    SharedPreferences.Editor editor = sharedPref.edit();
                                    editor.putString("sessionKey", kdcReply.getSessionKey());
                                    editor.putString("peerIp", kdcReply.getPeerIp());
                                    editor.putString("peerName", peerName);
                                    editor.putString("initialMessage", message.getText().toString());
                                    editor.commit();

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

        return super.onOptionsItemSelected(item);
    }

    // Server Code
    private class ServerReceiver implements Runnable {
        private ServerSocket sSocket;
        private Socket cSocket;
        private BufferedReader in;
        private PrintWriter pw;
        private String buffer;
        private boolean running = true;

        public void sendMessage(String s) {
            pw.println(s);
            pw.flush();
        }

        public void killIt() {
            try {
                pw.close();
                in.close();
                cSocket.close();
                sSocket.close();
            } catch (IOException e) {
                e.printStackTrace();
            }

            running = false;
        }

        @Override
        public void run() {
            try {
                sSocket = new ServerSocket(9000);
                cSocket = sSocket.accept();
                pw = new PrintWriter(cSocket.getOutputStream(), true);
                while (running) {
                    in = new BufferedReader(new InputStreamReader(cSocket.getInputStream()));
                    buffer = in.readLine();

                    // Todo handle messages
                    serverHandleIncomingRequest(buffer);
                    if (buffer.equals("exit")) {
                        running = false;
                        pw.close();
                        in.close();
                    }
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }


    private void serverHandleIncomingRequest(String msg){

        //Todo msg = ticket+|+encrypted(nonce)
        // verify ticket decrypt nonce and reply with encrypted(nonce+nonce)



    }

}


