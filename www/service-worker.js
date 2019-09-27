let lastNotification = null;

self.addEventListener('push', function(event) {
	event.waitUntil(
		fetch("/check-notifications?format=push")
			.then(res => res.json())
			.then(notifications => {
				if(!notifications) return;
				for(var i=0; i < notifications.length; i++) {
					if(lastNotification === notifications[i]._id) {
						lastNotification = notifications[i]._id;
						return;
					}
					self.registration.showNotification(notifications[i].message);
				}
			})
	);
});

self.addEventListener('notificationclick', function(event) {
	event.waitUntil(clients.openWindow("/push/go-inbox"));
});

self.addEventListener('install', (event) => { self.skipWaiting() });
