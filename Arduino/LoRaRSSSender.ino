#include <SPI.h>
#include <LoRa.h>
#include <Arduino.h>  //not sure if necessary


int counter = 0;
long time1 = 0;
long time2 = 0;

const int csPin = 7;     // LoRa radio chip select
const int resetPin = 6;  // LoRa radio reset
const int irqPin = 1;    // change for your board; must be a hardware interrupt pin

void setup() {
#ifdef __DEBUG__
  dbg_start();
#endif

  delay(5000);
  Serial.begin(9600);
  while (!Serial)
    ;

  Serial.println("LoRa Desktop Setup Debugger:");

  if (!LoRa.begin(868E6)) {  // 915E6 US
    Serial.println("Starting LoRa failed!");
    while (1)
      ;
  }
}
void send() {

  while (1) {
    delay(3000);
    Serial.print("Sending packet: ");
    Serial.println(counter);

    // send packet
    LoRa.beginPacket();
    LoRa.print("Desktop ");

    LoRa.print(counter);
    LoRa.endPacket();
    //Serial.print("Whats happening here? 26");
    //time1 = micros();
    //Serial.print("Whats happening here? 28");
    counter++;
    Serial.println("Sent Message. leaving send function");
    //delay(5000);
  }
}

void loop() {

  Serial.println("Sending:");
  send();
}
