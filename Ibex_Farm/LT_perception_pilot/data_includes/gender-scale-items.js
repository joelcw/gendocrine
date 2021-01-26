var defaults = [
    "Scale", {
        html: { html: "<u>Rate yourself on a sliding scale</u>" },
        startValue: 0,
        endValue: 100,
        startColor: "#ffffff",
        endColor: "#8c8c8c",
        leftLabel: "<Not at all>", 
        rightLabel: "<Completely>",
        hideProgressBar: true,
        saveReactionTime: false
    }
];

var items = [

    ["demog", "Scale", {startColor: "#ffffff", endColor: "#cc79a7", 
                           html: "How feminine do you consider yourself?<br><br>&#8592;Not at all <span style=\"display:inline-block; width: 100;\"></span> Completely&#8594;"}],
    ["demog", "Scale", {startColor: "#ffffff", endColor: "#4fa9d6", 
                           html: "How masculine do you consider yourself?<br><br>&#8592;Not at all <span style=\"display:inline-block; width: 100;\"></span> Completely&#8594;"}],
    ["demog", "Scale", {startColor: "#ffffff", endColor: "#c6960f", 
                           html: "How genderless do you consider yourself?<br><br>&#8592;Not at all <span style=\"display:inline-block; width: 100;\"></span> Completely&#8594;"}]

];