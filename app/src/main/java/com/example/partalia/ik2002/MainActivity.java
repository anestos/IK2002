package com.example.partalia.ik2002;

import android.content.Context;
import android.os.Bundle;
import android.app.Activity;
import android.content.Intent;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;


import java.io.FileOutputStream;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.security.Key;
import java.security.Security;
import java.security.spec.KeySpec;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

public class MainActivity extends Activity {

    private Button btnGenerate;
    private EditText txtName;
    private EditText txtPassword;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        btnGenerate = (Button) findViewById(R.id.btnJoin);
        txtName = (EditText) findViewById(R.id.name);
        txtPassword = (EditText) findViewById(R.id.password);

        // Hiding the action bar
        getActionBar().hide();

        btnGenerate.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                if (txtName.getText().toString().trim().length() > 0 && txtPassword.getText().toString().length() > 0) {

                    String name = txtName.getText().toString().trim();
                    String password = txtPassword.getText().toString();

                    Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider());
                    String ip = getIPAddress();

                    byte[] salt = ip.getBytes();

                    try {
                        SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
                        KeySpec keyspec = new PBEKeySpec(password.toCharArray(), salt, 4000, 256);
                        Key key = factory.generateSecret(keyspec);

                        String filename = "keystore.txt";
                        String string = Arrays.toString(key.getEncoded());
                        Context context = getApplicationContext();
                       // File file = new File(context.getFilesDir(), filename);

                        FileOutputStream outputStream;
                        outputStream = openFileOutput(filename, Context.MODE_PRIVATE);
                        outputStream.write(string.getBytes());
                        outputStream.close();

                    } catch (Exception e) {
                        e.printStackTrace();
                    }


                    Intent intent = new Intent(MainActivity.this,
                            InitialScreen.class);
                    intent.putExtra("name", name);

                    startActivity(intent);

                } else {
                    Toast.makeText(getApplicationContext(),
                            "Please enter your name", Toast.LENGTH_LONG).show();
                }
            }
        });
    }
    public static String getIPAddress() {
        try {
            List<NetworkInterface> interfaces = Collections.list(NetworkInterface.getNetworkInterfaces());
            for (NetworkInterface intf : interfaces) {
                List<InetAddress> addrs = Collections.list(intf.getInetAddresses());
                for (InetAddress addr : addrs) {
                    if (!addr.isLoopbackAddress()) {
                        return addr.getHostAddress().toUpperCase();
                    }
                }
            }
        } catch (Exception ex) { } // for now eat exceptions
        return "";
    }
}
