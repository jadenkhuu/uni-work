import { BACKEND_PORT } from './config.js';
// A helper you may want to use when uploading new images to the server.
import { fileToDataUrl, api_call_post, api_call_get, api_call_put, show_error, close_error, api_call_delete } from './helpers.js';
import { load_dashboard, clear_channel_details, clear_dashboard } from './dashboard.js'
import { get_channel_details } from './channel.js'
import { get_messages, send_message, edit_message } from './messages.js';

export let globalToken = null;
export let globalUserId = null;
export let selectedChannelId = null;
export let globalMsgId = null;

// Page display - block/flex
export const show_page = (pageName) => {
    clear_dashboard()
    for (const page of document.querySelectorAll('.page-block')) {
        page.style.display = 'none';
    }
    document.getElementById(`page-${pageName}`).style.display = 'block';

    if (pageName === 'login' 
        || pageName === 'register' 
        || pageName === 'createChannel' 
        || pageName === 'editChannel') {
        document.getElementById(`page-${pageName}`).style.display = 'flex';
    } 
    else if (pageName === 'dashboard') {
        load_dashboard()
    }
};

// Close error popup
document.addEventListener('DOMContentLoaded', function() {
    const closeButton = document.getElementById('error-exit');
    closeButton.addEventListener('click', close_error);
});

// POST auth/login
document.getElementById("login-submit").addEventListener('click', () => {
    const email = document.getElementById("login-email").value;
    const password = document.getElementById("login-password").value;
    api_call_post('auth/login', {
        email: email,
        password: password,
    })
    .then((body) => {
        console.log("login body")
        console.log(body)
        globalUserId = body.userId
        globalToken = body.token;
        show_page('dashboard');
    })
    .catch((msg) => {
        show_error(msg)
    });
});
// POST auth/register
document.getElementById("register-submit").addEventListener('click', () => {
    const email = document.getElementById("reg-email").value;
    const name = document.getElementById("reg-name").value;
    const password = document.getElementById("reg-password").value;
    const confirmPassword = document.getElementById("reg-confirm-password").value;

    if (password !== confirmPassword) {
        show_error('Passwords do not match');
    } else {
        api_call_post('auth/register', {
            email: email,
            name: name,
            password: password,
        })
        .then((body) => {
            globalUserId = body.userId
            globalToken = body.token;
            show_page('dashboard');
        })
        .catch((msg) => {
            // alert(msg);
            show_error(msg)
        });
    }
});

// POST auth/logout
document.getElementById("logout-button").addEventListener('click', () => {
    api_call_post('auth/logout', {}, true)
    .then(() => {
        globalToken = null;
        show_page('login');
    }).catch((msg) => {
        // alert(msg);
        show_error(msg)
    });
});

// POST channel
document.getElementById("create-channel-button").addEventListener('click', () => {
    document.getElementById("chan-name").value = "";
    document.getElementById("chan-desc").value = "";
    let privated = document.getElementById('private-check')
    privated.checked = false;
    show_page("createChannel")
});
document.getElementById("chan-submit").addEventListener('click', () => {
    const name = document.getElementById("chan-name").value;
    const desc = document.getElementById("chan-desc").value;
    let privated = document.getElementById('private-check').checked;
    api_call_post('channel', {
        name: name,
        private: privated,  
        description: desc,
    }, true)
    .then((body) => {      
        show_page('dashboard');
    })
    .catch((msg) => {
        // alert(msg);
        show_error(msg)
    });
});

// GET channel/{channelId}
document.getElementById('channel-list').addEventListener('click', function(event) {
    if (event.target.classList.contains('indiv-channel')) {
        const channelId = event.target.getAttribute('data');
        selectedChannelId = channelId;
        api_call_get('channel', {}, true)
        .then((body) => {
            const channel = body.channels.find(ch => ch.id === parseInt(selectedChannelId))
            if (channel && channel.members.includes(parseInt(globalUserId))) {
                // user is in the channel
                api_call_get(`channel/${selectedChannelId}`, {}, true)
                .then((body) => {
                    clear_channel_details();
                    get_channel_details(body);
                })
                .catch((msg) => {
                    show_error(msg);
                });
            } else {
                document.getElementById("page-join-page").style.display = 'flex';
            }
        })
        .catch((msg) => {
            show_error(msg)
        });
    }
});

// POST channel/{channelId}/join
document.getElementById('join-channel-btn').addEventListener('click', () => {
    api_call_post(`channel/${selectedChannelId}/join`, {}, true)
    .then((body) => {
        // user is in the channel
        api_call_get(`channel/${selectedChannelId}`, {}, true)
        .then((body) => {
            get_channel_details(body)
        })
        .catch((msg) => {
            show_error(msg)
        });
    })
    .catch((msg) => {
        show_error(msg)
    });
});

// POST channel/{channelId}/leave
document.getElementById("leave-channel-button").addEventListener('click', () => {
    api_call_post(`channel/${selectedChannelId}/leave`, {}, true)
    .then((body) => {
        document.getElementById("page-join-page").style.display = 'flex';
        // clear_channel_details()
        // clear_dashboard()
        load_dashboard()
    })
    .catch((msg) => {
        show_error(msg);
    });
});

// PUT channel/{channelId} - edit channel
document.getElementById("edit-channel-button").addEventListener('click', () => {
    document.getElementById("edit-name").value = "";
    document.getElementById("edit-desc").value = "";
    show_page("editChannel");
});
document.getElementById("edit-submit").addEventListener('click', () => {
    const name = document.getElementById("edit-name").value;
    const desc = document.getElementById("edit-desc").value;
    console.log({name, desc});
    console.log(selectedChannelId)
    api_call_put(`channel/${selectedChannelId}`, {
        name: name,
        description: desc,
    }, true)
    .then((body) => {
        show_page('dashboard');
    })
    .catch((msg) => {
        show_error(msg)
    });
});

// POST message/{channelId} - send message
document.getElementById("msg-send-button").addEventListener('click', () => {
    send_message();
});
document.getElementById("message-container").addEventListener('submit', () => {
    send_message();
});
document.getElementById("message-container").addEventListener('keydown', (event) => {
    if (event.key === 'Enter') {
        send_message();
    }
});

document.getElementById("edit-block").addEventListener('keydown', (event) => {
    if (event.key === 'Enter') {
        edit_message();
        document.getElementById("page-msg-edit").style.display = "none";
    }
});

document.getElementById("edit-msg-submit").addEventListener('click', () => {
    edit_message()
    document.getElementById("page-msg-edit").style.display = "none";
});

document.getElementById("main-messages").addEventListener('click', function(event) {
    const msgId = event.target.getAttribute('data');
    globalMsgId = msgId
    if (event.target.classList.contains('msg-delete')) {
        api_call_delete(`message/${selectedChannelId}/${msgId}`, {}, true)
        .then((body) => {
            document.getElementById("message-text").setAttribute("placeholder", "Send a message");
            get_messages();
        })
        .catch((msg) => {
            show_error(msg)
        });
    }
    if (event.target.classList.contains('msg-edit')) {
        document.getElementById("page-msg-edit").style.display = "flex";
    }
});

// Hyperlink function
for (const redirect of document.querySelectorAll('.redirect')) {
	const newPage = redirect.getAttribute('redirect');
	redirect.addEventListener('click', () => {
		show_page(newPage);
	});
};

if (globalToken === null) {
	show_page('login');
} else {
	show_page('dashboard');
}