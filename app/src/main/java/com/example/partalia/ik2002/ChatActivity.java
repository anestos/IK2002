package com.example.partalia.ik2002;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.media.Ringtone;
import android.media.RingtoneManager;
import android.net.Uri;
import android.os.Bundle;
import android.util.Base64;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.Toast;

import org.bouncycastle.util.Arrays;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.spec.InvalidParameterSpecException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;


public class ChatActivity extends Activity {
    private Button btnSend;
    private EditText inputMsg;

    private MessagesListAdapter adapter;
    private List<Message> listMessages;
    private ListView listViewMessages;
    private String name;
    private String peerName;
    ServerReceiver socketServerThread;
    ClientReceiver socketClientThread;
    private ExecutorService ex;
    private String peerIp;
    private String initialMessageToSend;
    private String initialMesssage;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_chat);

        btnSend = (Button) findViewById(R.id.btnSend);
        inputMsg = (EditText) findViewById(R.id.inputMsg);
        listViewMessages = (ListView) findViewById(R.id.list_view_messages);

        SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
        name = sharedPref.getString("user_name", "nada");
        peerName = sharedPref.getString("peerIp", "empty");
        peerIp = sharedPref.getString("peerName", "empty");
        initialMesssage = sharedPref.getString("initialMessage", "empty");

        listMessages = new ArrayList<Message>();

        adapter = new MessagesListAdapter(this, listMessages);
        listViewMessages.setAdapter(adapter);

        // Todo  start communication socket (handle incoming messages)

        // Todo when to start it

        ex = Executors.newFixedThreadPool(10);
        //bob
        if (initialMesssage.equals("empty")) {
            socketServerThread = new ServerReceiver();
            ex.submit(socketServerThread);
        } else {
            //alice
            initialMessageToSend = encrypt_message(initialMesssage);
            socketClientThread = new ClientReceiver(initialMessageToSend);
            ex.submit(socketClientThread);
            //send
            Message im = new Message(name, initialMesssage, true);
            appendMessage(im);
        }

        btnSend.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                // Sending message to web socket server
                Message m = new Message(name, inputMsg.getText().toString(), true);
                appendMessage(m);
                if (initialMesssage.equals("empty")) {
                    PrintWriter writer = socketServerThread.getPw();
                    String encrypted = encrypt_message(m.getMessage());
                    writer.println(encrypted);
                } else {
                    DataOutputStream writer = socketClientThread.getInput();
                    String encrypted = encrypt_message(m.getMessage());
                    try {
                        writer.writeUTF(encrypted);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
                // Clearing the input filed once message was sent
                inputMsg.setText("");
            }
        });

    }



    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_chat, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_close_conversation) {
            //Todo close session
            Intent intent = new Intent(ChatActivity.this,
                    InitialScreen.class);

            startActivity(intent);

        }
        if (id == R.id.action_delete_key) {
            //Todo close session
            SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
            SharedPreferences.Editor editor = sharedPref.edit();
            editor.putString("user_name", "nada");
            editor.putString("user_key", "key");
            editor.commit();

            Intent intent = new Intent(ChatActivity.this,
                    MainActivity.class);

            startActivity(intent);
            return true;
        }

        return super.onOptionsItemSelected(item);
    }
    /**
     * Appending message to list view
     * */
    private void appendMessage(final Message m) {
        runOnUiThread(new Runnable() {

            @Override
            public void run() {
                listMessages.add(m);

                adapter.notifyDataSetChanged();

                // Playing device's notification
                playBeep();
            }
        });
    }

    private void showToast(final String message) {

        runOnUiThread(new Runnable() {

            @Override
            public void run() {
                Toast.makeText(getApplicationContext(), message,
                        Toast.LENGTH_LONG).show();
            }
        });

    }

    public void playBeep() {

        try {
            Uri notification = RingtoneManager
                    .getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
            Ringtone r = RingtoneManager.getRingtone(getApplicationContext(),
                    notification);
            r.play();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Server Code
    private class ServerReceiver implements Runnable {
        private ServerSocket sSocket;
        private Socket cSocket;
        private BufferedReader in;
        private PrintWriter pw;
        private String buffer;
        private boolean running = true;

        public PrintWriter getPw() {
            return pw;
        }
        public void killIt(){
            running = false;
        }

        @Override
        public void run() {
            try {
                sSocket = new ServerSocket(9001);
                cSocket = sSocket.accept();
                pw = new PrintWriter(cSocket.getOutputStream(), true);
                in = new BufferedReader(new InputStreamReader(cSocket.getInputStream()));
                while (running) {

                    buffer = in.readLine();

                    // Todo decrypt to buffer kai constract Message add sti lista
                    decrypt_and_show(buffer);
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

    // Client Code
    private class ClientReceiver implements Runnable {
        private Socket cSocket;
        private BufferedReader in;
        private PrintWriter pw;
        private String buffer;
        private boolean running = true;
        private DataOutputStream out;
        private String msg;
        private BufferedReader input;


        public ClientReceiver (String msg){
            this.msg = msg;

        }


        public DataOutputStream getInput() {
            return out;
        }
        public void killIt(){
            running = false;
        }

        @Override
        public void run() {
            try {
                InetAddress serverAddr =InetAddress.getByName(peerIp);
                cSocket = new Socket(peerIp, 9001);
                out = new DataOutputStream(cSocket.getOutputStream());

                InputStreamReader inputStream = new InputStreamReader(cSocket.getInputStream());
                input = new BufferedReader(inputStream);

                out.writeUTF(msg);

                while (running && serverAddr.isReachable(1000)) {

                    buffer = input.readLine();
                    decrypt_and_show(buffer);

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

    private Boolean decrypt_and_show(String buffer) {
        System.out.println("Decrypting: "+buffer);

        byte[] bufferDec = org.bouncycastle.util.encoders.Base64.decode(buffer);

        SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
        String stringedKey = sharedPref.getString("sessionKey", "key");
        byte[] decodedKey = Base64.decode(stringedKey.getBytes(), Base64.DEFAULT);
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

            Message msg = new Message(peerName, new String(decrypted), false);
            appendMessage(msg);

        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (NoSuchProviderException e) {
            e.printStackTrace();
        } catch (NoSuchPaddingException e) {
            e.printStackTrace();
        } catch (InvalidAlgorithmParameterException e) {
            e.printStackTrace();
        } catch (InvalidKeyException e) {
            e.printStackTrace();
        } catch (BadPaddingException e) {
            e.printStackTrace();
        } catch (IllegalBlockSizeException e) {
            e.printStackTrace();
        }
return true;

    }
    private String encrypt_message(String message) {
        Cipher cipher;
        SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
        String stringedKey = sharedPref.getString("sessionKey", "key");
        byte[] decodedKey = Base64.decode(stringedKey.getBytes(), Base64.DEFAULT);
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

        byte[] everything = Arrays.concatenate(iv,output);
        String toSend =  Base64.encodeToString(everything, Base64.DEFAULT);

        return toSend;

    }

}
