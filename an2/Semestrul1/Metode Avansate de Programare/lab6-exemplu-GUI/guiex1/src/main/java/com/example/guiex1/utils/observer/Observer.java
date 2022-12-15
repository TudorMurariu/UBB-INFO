package com.example.guiex1.utils.observer;


import com.example.guiex1.utils.events.Event;

public interface Observer<E extends Event> {
    void update(E e);
}