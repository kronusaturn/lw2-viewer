let cache = null;

self.addEventListener('fetch', function(event) {
	if(event.request.url.match(/^\/([a-z]*\.(js|css)|css\/|assets\/)/)) {
		let responder = cache => {
			return cache.match(event.request).then(match => {
				if(match) {
					return match;
				} else {
					return cache.match(event.request, {ignoreSearch: true}).then(match => {
						if(match) cache.delete(match);
						
						return fetch(event.request).then(response => {
							cache.put(event.request, response.clone());
							return response;
						});
					});
				}
			});
		};

		if(cache) {
			event.respondWith(responder(cache));
		} else {
			event.respondWith(
				caches.open("v1").then(openedCache => {
					cache = openedCache;
					return responder(cache);
				})
			);
		}
	}
})

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

self.addEventListener('install', (event) => {
	event.waitUntil(
		caches.open("v1").then(openedCache => { cache = openedCache; }).then(self.skipWaiting())
	);
});

self.addEventListener('activate', (event) => {
	event.waitUntil(clients.claim());
});
