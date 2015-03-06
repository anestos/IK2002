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

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
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
    String serverMessage = "";
    ServerSocket serverSocket;
    Thread socketServerThread;
    private volatile boolean serverRunning = true;

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

        // Todo server start
        socketServerThread = new Thread(new SocketServerThread());
        socketServerThread.start();


        btnContact.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {

                if (txtPeerName.getText().toString().trim().length() > 0 && message.getText().toString().length() > 0) {

                    // Todo Server stop
                   /* serverRunning = false;
                    try {
                        socketServerThread.join();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }*/

                    String myName = name;
                    String peerName = txtPeerName.getText().toString();

                    Toast.makeText(getApplicationContext(), "Please wait, contacting "+peerName, Toast.LENGTH_LONG).show();

                    String toEncrypt = "";
                    byte[] toSend;
                    byte[] random = new byte[8];
                    SecureRandom rd = new SecureRandom();
                    rd.nextBytes(random);

                    byte[] rndEnc = org.bouncycastle.util.encoders.Base64.encode(random);
                    System.out.println("Nonce: " + new String(rndEnc));
                    System.out.println("Nonce length:"+ rndEnc.length);
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

                        ExecutorService executor = Executors.newFixedThreadPool(1);
                        Callable<String> callable = new Sender(encoded, serverIp , 8080, false);
                        Future<String> send = executor.submit(callable);

                        KdcReply kdcReply = new KdcReply(send, key);

                        if (new String(rndEnc).equals(kdcReply.getNonce()) && peerName.equals(kdcReply.getPeerName())) {
                            // Todo Do handshake with peer
                            System.out.println("Do handshake");

                        }
                        // if handhsake complete
                        //Todo if handshake is completed correctly
                        SharedPreferences sharedPref = getSharedPreferences("myStorage",Context.MODE_PRIVATE);
                        SharedPreferences.Editor editor = sharedPref.edit();
                        editor.putString("sessionKey", kdcReply.getSessionKey());
                        editor.putString("peerIp", kdcReply.getPeerIp());
                        editor.putString("peerName", peerName);
                        editor.putString("initialMessage", message.getText().toString());
                        editor.commit();

                        Intent intent = new Intent(InitialScreen.this, ChatActivity.class);
                        startActivity(intent);


                    } catch (NoSuchAlgorithmException e) {
                        e.printStackTrace();
                    } catch (NoSuchProviderException e) {
                        e.printStackTrace();
                    } catch (NoSuchPaddingException e) {
                        e.printStackTrace();
                    } catch (InvalidKeyException e) {
                        e.printStackTrace();
                    } catch (InvalidParameterSpecException e) {
                        e.printStackTrace();
                    } catch (BadPaddingException e) {
                        e.printStackTrace();
                    } catch (IllegalBlockSizeException e) {
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
    private class SocketServerThread extends Thread {

        static final int SocketServerPORT = 9002;
        int count = 0;

        @Override
        public void run() {
            if (serverRunning) {
                try {
                    serverSocket = new ServerSocket(SocketServerPORT);
                    InitialScreen.this.runOnUiThread(new Runnable() {

                        @Override
                        public void run() {
                            System.out.println("I'm waiting here: "
                                    + serverSocket.getLocalPort());
                        }
                    });

                    while (true) {
                        Socket socket = serverSocket.accept();
                        count++;
                        serverMessage += "#" + count + " from " + socket.getInetAddress()
                                + ":" + socket.getPort() + "\n";

                        InitialScreen.this.runOnUiThread(new Runnable() {

                            @Override
                            public void run() {
                                System.out.println(serverMessage);
                            }
                        });

                        SocketServerReplyThread socketServerReplyThread = new SocketServerReplyThread(
                                socket, count);
                        socketServerReplyThread.run();

                    }
                } catch (IOException e) {
                    e.printStackTrace();
                    System.out.println("failed to open socket");
                }
            }
        }

    }

    private class SocketServerReplyThread extends Thread {

        private Socket hostThreadSocket;
        int cnt;

        SocketServerReplyThread(Socket socket, int c) {
            hostThreadSocket = socket;
            cnt = c;
        }

        @Override
        public void run() {
            OutputStream outputStream;
            String msgReply = "Hello from Android, you are #" + cnt;

            try {
                outputStream = hostThreadSocket.getOutputStream();
                PrintStream printStream = new PrintStream(outputStream);
                printStream.print(msgReply);
                printStream.close();

                serverMessage += "replayed: " + msgReply + "\n";

                InitialScreen.this.runOnUiThread(new Runnable() {

                    @Override
                    public void run() {
                        System.out.println(serverMessage);
                    }
                });

            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
                serverMessage += "Something wrong! " + e.toString() + "\n";
            }

            InitialScreen.this.runOnUiThread(new Runnable() {

                @Override
                public void run() {
                    System.out.println(serverMessage);
                }
            });
        }

    }

    private String getIpAddress() {
        String ip = "";
        try {
            Enumeration<NetworkInterface> enumNetworkInterfaces = NetworkInterface
                    .getNetworkInterfaces();
            while (enumNetworkInterfaces.hasMoreElements()) {
                NetworkInterface networkInterface = enumNetworkInterfaces
                        .nextElement();
                Enumeration<InetAddress> enumInetAddress = networkInterface
                        .getInetAddresses();
                while (enumInetAddress.hasMoreElements()) {
                    InetAddress inetAddress = enumInetAddress.nextElement();

                    if (inetAddress.isSiteLocalAddress()) {
                        ip += "SiteLocalAddress: "
                                + inetAddress.getHostAddress() + "\n";
                    }

                }

            }

        } catch (SocketException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            ip += "Something Wrong! " + e.toString() + "\n";
        }

        return ip;
    }
}


