var shuffleSequence = seq("intro", sepWith("sep", seq("practice")), rshuffle("cont","cont","cont"), "exit");
var practiceItemTypes = ["practice"];

var defaults = [
    "Separator", {
        transfer: 1000,
        normalMessage: "Please wait for the next sentence.",
        errorMessage: "Wrong. Please wait for the next sentence."
    },
    "Question", {
        q: 'What sound did you hear?<br>Press the number or click the letters.',
        as: ["s", "sh"],
        presentHorizonally: false,
        hasCorrect: false,
        randomOrder: false,
        showNumbers: true,
        autoFirstChar: false
    },
    "Message", {
        hideProgressBar: false
    },
    "Form", {
        hideProgressBar: false,
        continueOnReturn: true,
        saveReactionTime: true
    }
];

var items = [

    ["sep", "Separator", { }],

    ["intro", "Form", {
        html: { include: "example_intro.html" },
        validators: {
            age: function (s) { if (s.match(/^\d+$/)) return true; else return "Bad value for \u2018age\u2019"; }
        }
    } ],

    //
    // Practice items
    //
    ["practice", "Question", {hasCorrect: false, randomOrder: false,
                              q: "How would you like to answer this question?",
                              as: ["Press 1 or click here for this answer.",
                                   "Press 2 or click here for this answer.",
                                   "Press 3 or click here for this answer."]}],

// cont
    [["cont",1], "Message", {
    	html: {
    	      include: 'old_sa_01.html'
    	  }},
    	// Just to check if participant can hear audio or No
    	"Question", {}],
    [["cont",2], "Message", {
    	html: {
    	      include: 'old_sa_07.html'
    	  }},
    	// Just to check if participant can hear audio or No
    	"Question", {}],



    ["exit", "Question", {q: "This is the end of the experiment. Click 1 to prove you're human.",
                              as: ["Press 1 or click here for this answer.",
                                   "Press 2 or click here for this answer.",
                                   "Press 3 or click here for this answer."]}]

];