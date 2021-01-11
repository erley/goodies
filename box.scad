echo(version=version());

T=2;        // box case thickness
P=30;       // padding for speakers
S=5;        // box pin thickness

W=200-T*2;  // box inside width
L=60;       // box inside length
H=20;       // box inside height

// box pins
BOX_PINS=[    // x, y, 0
    [-W/2+S/2, -L/2+S/2, 0],
    [-W/2+S/2,  L/2-S/2, 0],
    [ W/2-S/2, -L/2+S/2, 0],
    [ W/2-S/2,  L/2-S/2, 0]
  ];

// pins for board
PCB_PINS=[    // x, y
    [14, 9],
    [13.5, 37],
    [119.5, 9],
    [125, 37]
  ];

// pins for speakers
PIN_OFFSET=10;
SPK_PINS=[    // height, x, y
    [4, 10-T, 30-PIN_OFFSET],
    [6, 25-T, 56-PIN_OFFSET],
    [5, 187+T, 56.5-PIN_OFFSET],
    [4, 189+T, 22-PIN_OFFSET]
  ];

module BOX() {
  difference () {
    // outside box
    translate ([0, 0, -T/2]) {
      cube([W+T*2, L+T*2, H+T], center=true);
    }
    // inside box space
    cube([W, L, H], center=true);
    // box connectors door
    translate ([0, L/2+T/2, 0]) {
      cube([W-P*2, T, H], center=true);
    }
  }
}

module BOX_PIN() {
  // pin hole
  Hh=5;
  Rh=1;
  difference() {
    cube([S,S,H], center=true);
    translate ([0, 0, H/2-Hh/2]) {
      cylinder(h=Hh, r1=Rh/2, r2=Rh/2, center=true);
    }
  }
}

module PCB_PIN(dim) {  // dim is [x, y] array
  // pin
  Hb=15;
  Rb1=8;
  Rb2=6;
  // pin hole
  Hh=5;
  Rh=1;
  translate ([-W/2+P+dim[0], -L/2+dim[1], -H/2+Hb/2]) {
    difference() {
      cylinder(h=Hb, r1=Rb1/2, r2=Rb2/2, center=true, $fa=6);
      translate ([0, 0, Hb/2-Hh/2]) {
        cylinder(h=Hh, r1=Rh/2, r2=Rh/2, center=true);
      }
    }
  }
}

module SPK_PIN(dim) {  // dim is [h, x, y] array
  // pin
  Rs=5;
  // pin hole
  Hh=3;
  Rh=1;
  translate ([-W/2+dim[1], -L/2+dim[2], -H/2+dim[0]/2]) {
    difference() {
      cylinder(h=dim[0], r1=Rs/2, r2=Rs/2, center=true, $fa=6);
      translate ([0, 0, dim[0]/2-Hh/2]) {
        cylinder(h=Hh, r1=Rh/2, r2=Rh/2, center=true);
      }
    }
  }
}

BOX();
for ( i = [1 : 4] ) {
  PCB_PIN(PCB_PINS[i-1]);
  SPK_PIN(SPK_PINS[i-1]);
  translate (BOX_PINS[i-1]) {
    BOX_PIN();
  }
}
