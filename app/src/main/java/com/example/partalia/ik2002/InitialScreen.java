package com.example.partalia.ik2002;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
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


public class    InitialScreen extends Activity {
    private Button btnContact;
    private EditText txtPeerName;
    private EditText txtServerIP;
    private EditText message;

   @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_initial_screen);

        String ret = "";

        try {
            InputStream inputStream = openFileInput("keystore.txt");

            if ( inputStream != null ) {
                InputStreamReader inputStreamReader = new InputStreamReader(inputStream);
                BufferedReader bufferedReader = new BufferedReader(inputStreamReader);
                String receiveString = "";
                StringBuilder stringBuilder = new StringBuilder();

                while ( (receiveString = bufferedReader.readLine()) != null ) {
                    stringBuilder.append(receiveString);
                }

                inputStream.close();
                ret = stringBuilder.toString();
            }

        } catch (FileNotFoundException e) {
            Intent intent = new Intent(InitialScreen.this,
                    MainActivity.class);

            startActivity(intent);
        } catch (IOException e) {
            e.printStackTrace();
        }

       btnContact = (Button) findViewById(R.id.btn_contact);
       txtPeerName = (EditText) findViewById(R.id.peer_name);
       txtServerIP = (EditText) findViewById(R.id.server_ip);
       message = (EditText) findViewById(R.id.message);

       // ToDo check gia empty
       // Todo Send request to server
       Socket socket = null;
       try {
           socket = new Socket("10.0.2.2", 8080);

       DataOutputStream dataOutput = new DataOutputStream(socket.getOutputStream());

       InputStreamReader inputstream = new InputStreamReader(socket.getInputStream());
       BufferedReader input = new BufferedReader(inputstream);

       } catch (IOException e) {
           e.printStackTrace();
       }

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
            Context context = getApplicationContext();
            context.deleteFile("keystore.txt");
            Intent intent = new Intent(InitialScreen.this,
                    MainActivity.class);

            startActivity(intent);
            return true;
        }

        return super.onOptionsItemSelected(item);
    }
}
