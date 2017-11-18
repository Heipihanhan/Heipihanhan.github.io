function setup() {
	createCanvas(1400, 1200);
}

function draw() {
	rectMode(CORNERS);
	background(0);
	title1();
	title2();
	newsRank1();
	newsRank2();
	newsRank3();
	newsRank4();
	newsRank5();
	newsRank6();
	newsRank7();
	newsRank8();
	newsRank9();
	newsRank10();

// 1. Lebanon
    if (mouseX > 600 && mouseX < 750 && mouseY > 100 && mouseY < 200){
    text('Lebanon', 1000, 30, 200, 200);
    text('2014 - 2015', 1000, 60, 200, 200);
    text('20048 total intensity ', 1000, 100, 200, 200);
    textSize(5);
    }

	if (mouseX > 800 && mouseX < 950 && mouseY > 100 && mouseY < 200){
	text('Lebanon', 1000, 30, 200, 200);
	text('2014 - 2015', 1000, 60, 200, 200);
    text('2,210,616 refugees and asylum seekers', 1000, 100, 200, 200);
    textSize(5);
    }

// 2. Jordan
    if (mouseX > 600 && mouseX < 750 && mouseY > 200 && mouseY < 300){
    text('Jordan', 1000, 30, 200, 200);
    text('2014 - 2015', 1000, 60, 200, 200);
    text('19155 total intensity ', 1000, 100, 200, 200);
    textSize(5);
    }

	if (mouseX > 800 && mouseX < 950 && mouseY > 200 && mouseY < 300){
	text('Jordan', 1000, 30, 200, 200);
    text('2014 - 2015', 1000, 60, 200, 200);
    text('1,251,335 million refugees and asylum seekers', 1000, 100, 200, 250);
    textSize(5);
    }

// 3. Turkey
    if (mouseX > 600 && mouseX < 750 && mouseY > 300 && mouseY < 400){
    text('Turkey', 1000, 30, 200, 200);
    text('2014 - 2015', 1000, 60, 200, 200);
    text('16209 total intensity ', 1000, 100, 200, 200);
    textSize(5);
    }

	if (mouseX > 800 && mouseX < 950 && mouseY > 300 && mouseY < 400){
	text('Turkey', 1000, 30, 200, 200);
    text('2014 - 2015', 1000, 60, 200, 200);
    text('4,061,995 refugees and asylum seekers', 1000, 100, 200, 200);
    textSize(5);
    }

// 4. Iraq
    if (mouseX > 600 && mouseX < 750 && mouseY > 400 && mouseY < 500){
    text('Iraq', 1000, 30, 200, 200);
    text('2014 - 2015', 1000, 60, 200, 200);
    text('15317 total intensity ', 1000, 100, 200, 200);
    textSize(5);
    }

	if (mouseX > 800 && mouseX < 950 && mouseY > 400 && mouseY < 500){
	text('Iraq', 1000, 30, 200, 200);
    text('2014 - 2015', 1000, 60, 200, 200);
    text('480,063 refugees and asylum seekers', 1000, 100, 200, 200);
    textSize(5);
    }

// 5. Israel
    if (mouseX > 600 && mouseX < 750 && mouseY > 500 && mouseY < 600){
    text('Israel', 1000, 30, 200, 200);
    text('2014 - 2015', 1000, 60, 200, 200);
    text('8030 total intensity ', 1000, 100, 200, 200);
    textSize(5);
    }

	if (mouseX > 800 && mouseX < 950 && mouseY > 500 && mouseY < 600){
	text('Israel', 1000, 30, 200, 200);
    text('2014 - 2015', 1000, 60, 200, 200);
    text('0 refugees and asylum seekers', 1000, 100, 200, 200);
    textSize(5);
    }

// 6. U.K.
    if (mouseX > 600 && mouseX < 750 && mouseY > 600 && mouseY < 700){
    text('U.K.', 1000, 430, 200, 200);
    text('2014 - 2015', 1000, 460, 200, 200);
    text('7146 total intensity ', 1000, 500, 200, 200);
    textSize(5);
    }

	if (mouseX > 800 && mouseX < 950 && mouseY > 600 && mouseY < 700){
	text('U.K.', 1000, 430, 200, 200);
    text('2014 - 2015', 1000, 460, 200, 200);
    text('14,046 refugees and asylum seekers', 1000, 500, 200, 200);
    textSize(5);
    }

// 7. Germany
    if (mouseX > 600 && mouseX < 750 && mouseY > 700 && mouseY < 800){
    text('Germany', 1000, 530, 200, 200);
    text('2014 - 2015', 1000, 570, 200, 200);
    text('6943 total intensity ', 1000, 600, 200, 200);
    textSize(5);
    }

	if (mouseX > 800 && mouseX < 950 && mouseY > 700 && mouseY < 800){
    text('Germany', 1000, 530, 200, 200);
    text('2014 - 2015', 1000, 570, 200, 200);
    text('267,771 refugees and asylum seekers', 1000, 600, 200, 200);
    textSize(5);
    }

// 8. Greece
    if (mouseX > 600 && mouseX < 750 && mouseY > 800 && mouseY < 900){
    text('Greece', 1000, 530, 200, 200);
    text('2014 - 2015', 1000, 570, 200, 200);
    text('5907 total intensity ',1000, 600, 200, 200);
    textSize(5);
    }

	if (mouseX > 800 && mouseX < 950 && mouseY > 800 && mouseY < 900){
    text('Greece', 1000, 530, 200, 200);
    text('2014 - 2015', 1000, 570, 200, 200);
    text('13,644 refugees and asylum seekers', 1000, 600, 200, 200);
    textSize(5);
    }
// 9. France
    if (mouseX > 600 && mouseX < 750 && mouseY > 900 && mouseY < 1000){
    text('France', 1000, 530, 200, 200);
    text('2014 - 2015', 1000, 570, 200, 200);
    text('5453 total intensity ', 1000, 600, 200, 200);
    textSize(5);
    }

	if (mouseX > 800 && mouseX < 950 && mouseY > 900 && mouseY < 1000){
    text('France', 1000, 530, 200, 200);
    text('2014 - 2015', 1000, 570, 200, 200);
    text('12,011 refugees and asylum seekers', 1000, 600, 200, 200);
    textSize(5);
    }

// 10. Egypt
    if (mouseX > 600 && mouseX < 750 && mouseY > 1000 && mouseY < 1100){
    text('Egypt', 1000, 530, 200, 200);
    text('2014 - 2015', 1000, 570, 200, 200);
    text('5279 total intensity ', 1000, 600, 200, 200);
    textSize(5);
    }

	if (mouseX > 800 && mouseX < 950 && mouseY > 1000 && mouseY < 1100){
    text('Egypt', 1000, 530, 200, 200);
    text('2014 - 2015', 1000, 570, 200, 200);
    text('256,016 refugees and asylum seekers', 1000, 600, 200, 200);
    textSize(5);
    }

 
    

}

function title1() {
	fill('#F1C40F');
    textAlign(TOP, LEFT);
	textStyle(BOLD);
	textSize(20);
	text('News', 400, 20, 400, 50);
}

function title2() {
	fill('#F1C40F');
	textAlign(TOP, LEFT);
	textStyle(BOLD);
	textSize(20);
	text('Refugees & Asylum Seekers', 650, 20, 320, 50);
}


function newsRank1() {
	textAlign(CENTER, CENTER);
	textSize(20);
	text('Lebanon', 200, 100, 350, 100);
	ellipse(600, 150, 71, 71);
	// refugee
	ellipse(800, 150, 49, 49); 

}


function newsRank2() {
	textAlign(CENTER, CENTER);
	textSize(20);
	text('Jordan', 200, 200, 350, 100);
	ellipse(600, 250, 69, 69);
// refugee
	ellipse(800, 250, 37, 37);
}

function newsRank3() {
	textAlign(CENTER, CENTER);
	textSize(20);
	text('Turkey', 200, 300, 350, 100);
	ellipse(600, 350, 64, 64);
	// refugee
	ellipse(800, 350, 67, 67);

}

function newsRank4() {
	textAlign(CENTER, CENTER);
	textSize(20);
	text('Iraq', 200, 400, 350, 100);
	ellipse(600, 450, 62, 62);
	// refugee
	ellipse(800, 450, 23, 23);
}

function newsRank5() {
	textAlign(CENTER, CENTER);
	textSize(20);
	text('Israel', 200, 500, 350, 100);
	ellipse(600, 550, 45, 45);
	// refugee
	ellipse(800, 550, 0, 0);


}

function newsRank6() {
	textAlign(CENTER, CENTER);
	textSize(20);
	text('U.K.', 200, 600, 350, 100);
	ellipse(600, 650, 42, 42);
	// refugee
	ellipse(800, 650, 4, 4,);
}

function newsRank7() {
	textAlign(CENTER, CENTER);
	textSize(20);
	text('Germany', 200, 700, 350, 100);
	ellipse(600, 750, 42, 42);
// refugee
	ellipse(800, 750, 17, 17);

}

function newsRank8() {
	textAlign(CENTER, CENTER);
	textSize(20);
	text('Greece', 200, 800, 350, 100);
	ellipse(600, 850, 38, 38);
	// refugee
	ellipse(800, 850, 4, 4);
}

function newsRank9() {
	textAlign(CENTER, CENTER);
	textSize(20);
	text('France', 200, 900, 350, 100);
	ellipse(600, 950, 37, 37);
	// refugee
	ellipse(800, 950, 4, 4);
}

function newsRank10() {
	textAlign(CENTER, CENTER);
	textSize(20);
	text('Egypt', 200, 1000, 350, 100);
	ellipse(600, 1050, 36, 36);
// refugee
	ellipse(800, 1050, 17, 17);

}

