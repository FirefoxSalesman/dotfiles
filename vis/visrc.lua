--load standard vis runtime files, must be done before anything else
require('vis')

require('plugins/vis-sneak')
require('plugins/vis-surround')

--set keys & theme
vis.events.subscribe(vis.events.INIT, function()
	vis:command('set theme pywal')
end)
