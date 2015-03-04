package com.example.partalia.ik2002;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.util.Base64;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;


import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.Socket;
import java.nio.Buffer;
import java.security.AlgorithmParameters;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.InvalidParameterSpecException;
import java.security.spec.KeySpec;
import java.util.Arrays;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
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
        System.out.println(""+stringedKey+"|"+name);
        if (stringedKey.equals("key") || name.equals("nada")) {
            Intent intent = new Intent(InitialScreen.this, MainActivity.class);
            startActivity(intent);
        } else {
            byte[] decodedKey = Base64.decode(stringedKey.getBytes(), Base64.DEFAULT);
            SecretKey keytmp = new SecretKeySpec(decodedKey, 0, decodedKey.length, "PBKDF2WithHmacSHA1");
            key = new SecretKeySpec(keytmp.getEncoded(), "AES");
        }


        btnContact.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                // ToDo check gia empty
                String myName = name;
                String peerName = txtPeerName.getText().toString();
                String toEncrypt ="";
                byte[] toSend;
                byte[] random = new byte[8];
                SecureRandom rd = new SecureRandom();
                rd.nextBytes(random);

                toEncrypt = Arrays.toString(random)+"|"+myName+"|"+peerName;

                try {

                    Cipher cipher = Cipher.getInstance("AES/CTR/NoPadding", "BC");
                    cipher.init(Cipher.ENCRYPT_MODE, key);
                    AlgorithmParameters params = cipher.getParameters();
                    byte[] iv = params.getParameterSpec(IvParameterSpec.class).getIV();

                    byte[] output = cipher.doFinal(toEncrypt.getBytes());

                    System.out.println(""+Arrays.toString(output));

                    toSend = output;

                    Thread send = new Thread(new Sender(toSend, txtServerIP.getText().toString(), 8080, false));
                    send.start();

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



            }
        });
        // Todo send msg to peer
        // Todo go to chat activity and continue chatting
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
            SharedPreferences sharedPref = getSharedPreferences("myStorage",Context.MODE_PRIVATE);
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


