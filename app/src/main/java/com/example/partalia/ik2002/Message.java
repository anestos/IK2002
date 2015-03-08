package com.example.partalia.ik2002;

public class Message {
    private String fromName, message;
    private boolean isSelf;

    public Message(String fromName, String message, boolean isSelf) {
        this.fromName = fromName;
        this.message = message;
        this.isSelf = isSelf;
    }

    public String getFromName() {
        return fromName;
    }

    public String getMessage() {
        return message;
    }

    public boolean isSelf() {
        return isSelf;
    }
}