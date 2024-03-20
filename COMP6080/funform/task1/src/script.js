const street = document.getElementById('street-name');
const suburb = document.getElementById('suburb');
const postcode = document.getElementById('postcode');
const dob = document.getElementById('dob');

const boxes = document.getElementsByName('features');
const selectBtn = document.getElementById('select-all-btn');
const resetBtn = document.getElementById('reset-form');
const formResult = document.getElementById('form-result')
const building = document.getElementById('building-type');

function render() {
    formResult.value = validInput()
}

// check apartment or house
function buildingFunc() {
    if (building.value === "apartment") {
        return true
    } else if (building.value === "house") {
        return false
    }
}

// Age calculation func
function calculateAge() {
    // age calculation idea from GeeksForGeeks
    const d = dob.value.split("/")
    let date = d[0]
    let month = d[1]
    let year = d[2]

    const currentD = new Date()
    let curDate = parseInt(currentD.getDate());
    let curMonth = parseInt(currentD.getMonth()) + 1;
    let curYear = parseInt(currentD.getFullYear());

    const months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
    
    if (date > curDate) {
        curDate = curDate + months[month - 1];
        curMonth = curMonth - 1
    }
    if (month > curMonth) {
        curYear = curYear - 1;
        curMonth = curMonth + 12;
    }
    return parseInt(curYear - year);
}

// Check valid date
function validDate(dob) {
    let d = dob.split("/");
    d = '${d[1]}/${age[0]}/${d[2]}'
    return Date.parse(d);
}

// Concatenate features 
function featuresText() {
    let features = [];
    for (const box of boxes) {
        if (box.checked === true ) {
            features.push(box.value)
        }
    }
    if (features.length === 0) {
        return "no features";
    } else if (features.length === 1) {
        return features[0];
    } else if (features.length >= 2) {
        return features.slice(0, -1).join(", ") + ", and " + features[features.length - 1];
    }
}

// Check valid input
function validInput() {
    if (street.value.length < 3 || street.value.length > 50) {
        return "Please input a valid street name"
    }
    else if (suburb.value.length < 3 || suburb.value.length > 50) {
        return "Please input a valid suburb"
    }
    else if (/^[0-9]{4}$/.test(postcode.value) === false) {
        return "Please input a valid postcode"
    }
    else if (/^[0-9]{2}\/[0-9]{2}\/[0-9]{4}$/.test(dob.value) === false && isNaN(validDate(dob.value))) {
        return "Please input a valid date of birth"
    } 
    
    return `You are ${calculateAge()} years old, and your address is ${street.value} St, ${suburb.value}, ${postcode.value}, Australia. Your building is ${buildingFunc() ? "an" : "a"} ${building.value}, and it has ${featuresText()}.`
}

// Select button func   
function selectBtnFunc() {
    const allSelected = checkboxesFunc();
    selectBtn.value = allSelected ? "Select All": "Deselect All";
    for (const box of boxes) {
        box.checked = !allSelected;
    }
    render()
}
// if all are ticked, return true, if one or more unticked, return false
function checkboxesFunc() {
    let allSelected = true;
    for (const box of boxes) {
        if (box.checked == false) {
            allSelected = false
            break;
        }   
    }
    return allSelected;
}

// Checkboxes
for (const box of boxes) {
    box.addEventListener('change', () => {
        selectBtn.value = checkboxesFunc() ? "Deselect All" : "Select All";
        render();
    })
}

// Building type
building.addEventListener("click", () => { 
    building.addEventListener("change", () => {
        render();
    })  
})

street.addEventListener("blur", render)
suburb.addEventListener("blur", render) 
postcode.addEventListener("blur", render)
dob.addEventListener("blur", render)

// Select All Button
selectBtn.addEventListener("click", () => {
    selectBtnFunc();
})

// Reset button
resetBtn.addEventListener("click", () => {
    street.value = "";
    suburb.value = "";
    postcode.value = "";
    dob.value = ""; 
    for (const box of boxes) {
        box.checked = false;
    }
    selectBtn.value = checkboxesFunc() ? "Deselect All" : "Select All";
    formResult.value = "";
})
