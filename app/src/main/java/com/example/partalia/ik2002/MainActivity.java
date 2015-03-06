package com.example.partalia.ik2002;

import android.content.Context;
import android.content.SharedPreferences;
import android.net.wifi.WifiManager;
import android.os.Bundle;
import android.app.Activity;
import android.content.Intent;
import android.text.format.Formatter;
import android.util.Base64;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;
import java.security.Key;
import java.security.Security;
import java.security.spec.KeySpec;
import java.util.Arrays;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

public class MainActivity extends Activity {

    private Button btnGenerate;
    private EditText txtName;
    private EditText txtPassword;
    private EditText txtServerIP;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        btnGenerate = (Button) findViewById(R.id.btnJoin);
        txtName = (EditText) findViewById(R.id.name);
        txtPassword = (EditText) findViewById(R.id.password);
        txtServerIP = (EditText) findViewById(R.id.server_ip);


        // Hiding the action bar
        getActionBar().hide();


        btnGenerate.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                if (txtName.getText().toString().trim().length() > 0 && txtPassword.getText().toString().length() > 0 && txtServerIP.getText().toString().length() > 0) {

                    String name = txtName.getText().toString().trim();
                    String password = txtPassword.getText().toString();
                    String serverIp = txtServerIP.getText().toString();

                    Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider());

                    WifiManager wm = (WifiManager) getSystemService(WIFI_SERVICE);
                    String ip = Formatter.formatIpAddress(wm.getConnectionInfo().getIpAddress());

                    //Todo remove after local testing
                    //ip = "127.0.0.1";

                    System.out.println("ip: "+ ip);
                    byte[] salt = ip.getBytes();
                    System.out.println("salt: "+salt.length + " " + Arrays.toString(salt));


                    try {
                        SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
                        KeySpec keyspec = new PBEKeySpec(password.toCharArray(), salt, 4000, 256);
                        Key key = factory.generateSecret(keyspec);

                        String stringedKey = Base64.encodeToString(key.getEncoded(), Base64.DEFAULT);

                        SharedPreferences sharedPref = getSharedPreferences("myStorage",Context.MODE_PRIVATE);
                        SharedPreferences.Editor editor = sharedPref.edit();
                        editor.putString("user_name", name);
                        editor.putString("serverIp", serverIp);
                        editor.putString("user_key", stringedKey);
                        System.out.println("key:" +stringedKey);
                        editor.commit();

                    } catch (Exception e) {
                        e.printStackTrace();
                    }


                    Intent intent = new Intent(MainActivity.this,
                            InitialScreen.class);

                    startActivity(intent);

                } else {
                    Toast.makeText(getApplicationContext(),
                            "Please enter your name", Toast.LENGTH_LONG).show();
                }
            }
        });
    }
}
