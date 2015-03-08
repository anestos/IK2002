package com.ik2002.project.ik2002;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.media.Ringtone;
import android.media.RingtoneManager;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.Toast;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


public class ChatActivity extends Activity {
    private EditText inputMsg;

    private MessagesListAdapter adapter;
    private List<Message> listMessages;
    private ListView listViewMessages;
    private String name;
    private String peerName;
    ServerReceiver socketServerThread;
    ClientReceiver socketClientThread;
    private String peerIp;
    private String initialMesssage;
    private String stringedKey;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_chat);
        Killer.getInstance().setChatting(true);

        Button btnSend = (Button) findViewById(R.id.btnSend);
        inputMsg = (EditText) findViewById(R.id.inputMsg);
        listViewMessages = (ListView) findViewById(R.id.list_view_messages);

        SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
        name = sharedPref.getString("user_name", "nada");
        peerName = sharedPref.getString("peerName", "empty");
        peerIp = sharedPref.getString("peerIp", "empty");
        initialMesssage = sharedPref.getString("initialMessage", "empty");
        stringedKey = sharedPref.getString("sessionKey", "key");

        listMessages = new ArrayList<>();

        adapter = new MessagesListAdapter(this, listMessages);
        listViewMessages.setAdapter(adapter);

        ExecutorService ex = Executors.newFixedThreadPool(10);

        if (initialMesssage.equals("empty")) {
            socketServerThread = new ServerReceiver();
            ex.submit(socketServerThread);
        } else {
            String initialMessageToSend = CryptoUtil.encrypt(initialMesssage, stringedKey);
            socketClientThread = new ClientReceiver(initialMessageToSend);
            ex.submit(socketClientThread);
            Message im = new Message(name, initialMesssage, true);
            appendMessage(im);
        }

        btnSend.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {

                Message m = new Message(name, inputMsg.getText().toString(), true);
                appendMessage(m);

                if (initialMesssage.equals("empty")) {
                    String encrypted = CryptoUtil.encrypt(m.getMessage(), stringedKey);
                    socketServerThread.sendMessage(encrypted);
                } else {

                    String encrypted = CryptoUtil.encrypt(m.getMessage(), stringedKey);
                    socketClientThread.sendMessage(encrypted);
                }

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
        SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPref.edit();


        //noinspection SimplifiableIfStatement
        if (id == R.id.action_close_conversation) {
            // close session
            if (initialMesssage.equals("empty")) {
                socketServerThread.sendMessage(CryptoUtil.encrypt("exit", stringedKey));
            } else {
                socketClientThread.sendMessage(CryptoUtil.encrypt("exit", stringedKey));
            }

            Killer.getInstance().setChatting(false);
            Killer.getInstance().setRunning(true);

            editor.putString("sessionKey", "empty");
            editor.putString("peerIP", "empty");
            editor.putString("initialMessage", "empty");
            editor.commit();

            Intent intent = new Intent(ChatActivity.this, InitialScreen.class);
            startActivity(intent);

        }
        if (id == R.id.action_delete_key) {
            // close session
            Killer.getInstance().setChatting(false);
            Killer.getInstance().setRunning(true);
            if (initialMesssage.equals("empty")) {
                socketServerThread.sendMessage(CryptoUtil.encrypt("exit", stringedKey));
            } else {
                socketClientThread.sendMessage(CryptoUtil.encrypt("exit", stringedKey));
            }

            editor.putString("sessionKey", "empty");
            editor.putString("peerIP", "empty");
            editor.putString("initialMessage", "empty");
            editor.putString("user_name", "nada");
            editor.putString("user_key", "key");
            editor.commit();

            Intent intent = new Intent(ChatActivity.this, MainActivity.class);
            startActivity(intent);

            return true;
        }
        if (id == R.id.action_information) {
            Intent intent = new Intent(ChatActivity.this, Information.class);
            startActivity(intent);
        }

        return super.onOptionsItemSelected(item);
    }

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

        public void sendMessage(String s) {
            pw.println(s);
            pw.flush();
        }


        @Override
        public void run() {
            try {
                sSocket = new ServerSocket(9001);
                cSocket = sSocket.accept();
                pw = new PrintWriter(cSocket.getOutputStream(), true);
                while (Killer.getInstance().getChatting()) {
                    in = new BufferedReader(new InputStreamReader(cSocket.getInputStream()));

                    buffer = in.readLine();
                    decrypt_and_show(buffer, stringedKey);

                }
                pw.close();
                in.close();
                cSocket.close();
                sSocket.close();

            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }

    // Client Code
    private class ClientReceiver implements Runnable {
        private Socket cSocket;
        private PrintWriter pw;
        private String buffer;
        private String msg;
        private BufferedReader input;


        public ClientReceiver(String msg) {
            this.msg = msg;
        }

        public void sendMessage(String s) {
            pw.println(s);
            pw.flush();
        }


        @Override
        public void run() {
            try {
                cSocket = new Socket(peerIp, 9001);
                pw = new PrintWriter(new BufferedWriter(new OutputStreamWriter(cSocket.getOutputStream())), true);

                sendMessage(msg);

                while (Killer.getInstance().getChatting()) {
                    InputStreamReader inputStream = new InputStreamReader(cSocket.getInputStream());
                    input = new BufferedReader(inputStream);

                    buffer = input.readLine();
                    decrypt_and_show(buffer, stringedKey);
                }
                pw.close();
                input.close();
                cSocket.close();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }

    private void decrypt_and_show(String buffer, String sessionKey) {
        // if msg=exit, close connection
        String decrypted = CryptoUtil.decrypt(buffer, sessionKey);
        if (decrypted.equals("exit")) {
            Killer.getInstance().setChatting(false);
            showToast("Connection lost");
        } else {
            Message msg = new Message(peerName, decrypted, false);
            appendMessage(msg);
        }
    }


}
