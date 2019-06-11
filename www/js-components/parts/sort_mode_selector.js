`<div id='${section}-sort-mode-selector' class='sublevel-nav sort'>` + 
	Object.values(CommentSortMode).map(sortMode => 
		`<button type='button' class='sublevel-item sort-mode-${sortMode}' tabindex='-1' title='Sort by ${sortMode}'>${sortMode}</button>`
	).join("") +  
`</div>`