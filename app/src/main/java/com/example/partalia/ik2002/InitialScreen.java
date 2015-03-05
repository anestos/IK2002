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

import java.security.AlgorithmParameters;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.spec.InvalidParameterSpecException;
import java.util.Arrays;
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
    private EditText txtServerIP;
    private EditText message;
    private String name;
    private Key key;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_initial_screen);
        btnContact = (Button) findViewById(R.id.btn_contact);
        txtPeerName = (EditText) findViewById(R.id.peer_name);
        txtServerIP = (EditText) findViewById(R.id.server_ip);
        message = (EditText) findViewById(R.id.message);

        SharedPreferences sharedPref = getSharedPreferences("myStorage", Context.MODE_PRIVATE);
        name = sharedPref.getString("user_name", "nada");
        String stringedKey = sharedPref.getString("user_key", "key");

        String ret = "";
        System.out.println("" + stringedKey + "|" + name);
        if (stringedKey.equals("key") || name.equals("nada")) {
            Intent intent = new Intent(InitialScreen.this, MainActivity.class);
            startActivity(intent);
        } else {
            byte[] decodedKey = Base64.decode(stringedKey.getBytes(), Base64.DEFAULT);
            SecretKey keytmp = new SecretKeySpec(decodedKey, 0, decodedKey.length, "PBKDF2WithHmacSHA1");
            key = new SecretKeySpec(keytmp.getEncoded(), "AES");
        }

        // Todo server start


        btnContact.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {

                if (txtPeerName.getText().toString().trim().length() > 0 && txtServerIP.getText().toString().length() > 0 && message.getText().toString().length() > 0) {

                    // Todo Server stop

                    String myName = name;
                    String peerName = txtPeerName.getText().toString();

                    Toast.makeText(getApplicationContext(), "Please wait, contacting "+peerName, Toast.LENGTH_LONG).show();

                    String toEncrypt = "";
                    byte[] toSend;
                    byte[] random = new byte[8];
                    SecureRandom rd = new SecureRandom();
                    rd.nextBytes(random);

                    toEncrypt = Arrays.toString(random) + "|" + myName + "|" + peerName;

                    try {

                        Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding", "BC");
                        cipher.init(Cipher.ENCRYPT_MODE, key);
                        AlgorithmParameters params = cipher.getParameters();
                        byte[] iv = params.getParameterSpec(IvParameterSpec.class).getIV();
                        System.out.println("" + iv.length);

                        byte[] output = cipher.doFinal(toEncrypt.getBytes());

                        toSend = new byte[iv.length + output.length];
                        System.arraycopy(iv, 0, toSend, 0, iv.length);
                        System.arraycopy(output, 0, toSend, iv.length, output.length);


                        ExecutorService executor = Executors.newFixedThreadPool(1);
                        Callable<String> callable = new Sender(toSend, txtServerIP.getText().toString(), 8080, false);
                        Future<String> send = executor.submit(callable);

                        KdcReply kdcReply = new KdcReply(send, key);
                        if (kdcReply.getNonce().equals(random) && kdcReply.getPeerName().equals(peerName)) {
                            // Todo Do handshake with peer

                        }

                        //Todo if handshake is completed correctly
                        Intent intent = new Intent(InitialScreen.this, ChatActivity.class);
                        intent.putExtra("peerName", peerName);
                        intent.putExtra("message", message.getText().toString());
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
}


