float t = 0;

void setup(){
  size (600, 400);
}

void draw(){
  background(0);
  fill(255);
  t = t + 0.01;
  float x = noise(t);
  x = map(x,0,1,0,width);
  ellipse(x, height/2, 40, 40);
}
