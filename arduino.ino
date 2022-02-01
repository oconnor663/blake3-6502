void setup() {
  pinMode(13, OUTPUT);
}

void flash_num(int n) {
  for (int i = 0; i < n; i++) {
    digitalWrite(13, HIGH);
    delay(100);
    digitalWrite(13, LOW);
    delay(100);
  }
}

void loop() {
  int n = 0;
  while (1) {
    flash_num(n + 1);
    delay(1000);
    n = (n + 1) % 4;
  }
}
