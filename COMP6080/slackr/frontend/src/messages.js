import { api_call_get, api_call_post, api_call_put, format_date, show_error } from './helpers.js';
import { selectedChannelId, globalUserId, globalMsgId } from './main.js'

export const send_message = () => {
    const messageText = document.getElementById("message-text").value;
    if (messageText === "") {
        document.getElementById("message-text").value = "";
        get_messages()
        return;
    }
    api_call_post(`message/${selectedChannelId}`, {
        message: messageText,
        image: undefined,
    }, true)
    .then((body) => {
        document.getElementById("message-text").value = "";
        get_messages()
    })
    .catch((msg) => {
        // alert(msg);
        show_error(msg)
        // edit the html error box to display current error message 'msg'
        // change html here to show in the error pop up
    });
};

export const get_messages = () => {
    clear_messages();
    fetch_all_messages(0, 25, [])
    .then((allMessages) => {
        // console.log(allMessages)
        display_messages(allMessages, 0);
    })
    .catch((msg) => {
        // alert(msg);
        show_error(msg)
        // edit the html error box to display current error message 'msg'
        // change html here to show in the error pop up
    });
    document.getElementById("main-messages").style.display = 'flex';
    document.getElementById("message-container").style.display = 'flex';
};

export const clear_messages = () => {
    const messages = document.getElementById("main-messages");
    while (messages.firstChild) {
        messages.removeChild(messages.firstChild);
    }
    document.getElementById("main-messages").style.display = 'none';
    document.getElementById("message-container").style.display = 'none';
};
// recursively fetch all messages - referenced from forum
export const fetch_all_messages = (index, length, allMessages) => {
    return api_call_get(`message/${selectedChannelId}?start=${index}`, {}, true)
        // .then((res) => res.json())
        .then((body) => {
            const newMessages = [...allMessages, ...body.messages]
            if (body.messages.length > 0) {
                return fetch_all_messages(index + length, length, newMessages);
            }
            return newMessages;
        })
        .catch((msg) => {
            // alert(msg);
            show_error(msg)
            // edit the html error box to display current error message 'msg'
            // change html here to show in the error pop up
        });
};
// recursively fetch message to append to display
export const display_messages = (allMessages, index) => {
    if (allMessages.length === 0) {
        return;
    }
    api_call_get(`user/${allMessages[index].sender}`, {}, true)
    .then((body) => {
        const time_created= format_date(allMessages[index].sentAt);
        const messageContainer = generate_message(
            body.name,
            time_created,
            allMessages[index].message,
            allMessages[index].sender,
            allMessages[index].id,
        )
        document.getElementById("main-messages").appendChild(messageContainer);
        if (index + 1 < allMessages.length) {
            display_messages(allMessages, index + 1);
        }
    })
    .catch((msg) => {
        // alert(msg);
        show_error(msg)
        // edit the html error box to display current error message 'msg'
        // change html here to show in the error pop up
    });
};
// generate each message container
export const generate_message = (name, time, messageText, sentBy, id) => {
    // container div
    let container = document.createElement('div');
    container.className = 'message-box';
    container.setAttribute("data", id);

    // profile picture div
    let pic = document.createElement('div');
    pic.className = 'pic';
    container.appendChild(pic);

    // message content div
    let messageContent = document.createElement('div');
    messageContent.className = 'message-content';

    // name and time div
    let nameTime = document.createElement('div');
    nameTime.className = 'name-time';

    let nameSpan = document.createElement('span');
    nameSpan.className = 'name';
    nameSpan.textContent = name;
    nameSpan.style.fontWeight = "bold"

    let timeSpan = document.createElement('span');
    timeSpan.className = 'time';
    timeSpan.textContent = time;

    nameTime.appendChild(nameSpan);
    nameTime.appendChild(timeSpan);

    messageContent.appendChild(nameTime);

    // Add message
    let messageDiv = document.createElement('div');
    messageDiv.className = 'message';
    messageDiv.textContent = messageText;

    messageContent.appendChild(messageDiv);
    container.appendChild(messageContent);

    // edit react buttons on the side
    let msgButtons = document.createElement('div');
    msgButtons.className = 'msg-buttons';
    
    let reactBtn = document.createElement('i');
    reactBtn.className = 'bi bi-emoji-laughing msg-react'
    msgButtons.setAttribute("data", id);
    msgButtons.appendChild(reactBtn);

    let pinBtn = document.createElement('i');
    pinBtn.className = 'bi bi-pin msg-pin'
    pinBtn.setAttribute("data", id);
    msgButtons.appendChild(pinBtn);
    
    if (sentBy === globalUserId) {        
        let deleteBtn = document.createElement('i');
        deleteBtn.className = 'bi bi-trash3 msg-delete'
        deleteBtn.setAttribute("data", id);
        msgButtons.appendChild(deleteBtn);

        let editBtn = document.createElement('i');
        editBtn.className = 'bi bi-pencil msg-edit'
        editBtn.setAttribute("data", id);
        msgButtons.appendChild(editBtn);
    }

    container.appendChild(msgButtons);

    return(container)
};

export const edit_message = () => {
    const edited = document.getElementById("edit-msg").value;
    api_call_put(`message/${selectedChannelId}/${globalMsgId}`, {
        message: edited,
        image: undefined,
    }, true)
    .then((body) => {
        document.getElementById("edit-msg").value = ""
        document.getElementById("page-error").style.display = "none";
        get_messages();
    })
    .catch((msg) => {
        show_error(msg)
    });
}
