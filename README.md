# Flood Gate

Console application that acts as a rate limiting reverse proxy.

Whenever you have a service that imposes rate limits you can use this
proxy to throttle the requests that get through it in order to obey the
limits.

All the requests that hit above the rate limits get delayed until spare
timeslots appear again.

All the configured rate limits are monitored simultaneously, meaning
that you can both limit number of requests within an hour and within a
second. With this kind of configuration in place the requests get
delayed should you hit at least one of the limits.

There's a sample configuration for Etsy API in the
`docker/app/config.dhall` file.
