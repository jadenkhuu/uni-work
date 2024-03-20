import { globalUserId } from './main.js'
import { api_call_get, show_error} from './helpers.js';
import { clear_messages } from './messages.js'

// Dashboard Related
export const channel_style = (newChannel, channelId) => {
    newChannel.style.width = '100%';
    newChannel.style.height = '10%';
    newChannel.style.borderBottom = '2px solid white';
    newChannel.style.weight = 'bold'
    newChannel.style.padding = '6%';
    newChannel.setAttribute("class", "indiv-channel")
    newChannel.setAttribute("data", channelId)
};

// Clears channel details
// clears messages
export const clear_channel_details = () => {
    const channelDetails = document.getElementById("channel-details");
    while (channelDetails.firstChild) {
        channelDetails.removeChild(channelDetails.firstChild);
    }
    document.getElementById("leave-channel-button").style.display = 'none';
    document.getElementById("edit-channel-button").style.display = 'none';
    clear_messages();
};

// removes channels from list + clears channel details
export const clear_dashboard = () => {
    const element = document.getElementById("channel-list");
    while (element.firstChild) {
        element.removeChild(element.firstChild);
    }
    clear_channel_details();
};

// load dashboard (channel list)
export const load_dashboard = () => {
    clear_dashboard();
    api_call_get('channel', {}, true)
    .then((body) => {
        for (const channel of body.channels) {
            if (channel.private === false || (channel.private === true && channel.members.includes(globalUserId))) {
                const newChannel = document.createElement("div");
                newChannel.innerText = channel.name;
                channel_style(newChannel, channel.id)
                document.getElementById("channel-list").appendChild(newChannel);
            }
        };
    })
    .catch((msg) => {
        // alert(msg);
        show_error(msg)
        // edit the html error box to display current error message 'msg'
        // change html here to show in the error pop up
    });
};

