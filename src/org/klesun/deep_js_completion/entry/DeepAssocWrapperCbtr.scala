package org.klesun.deep_js_completion.entry

import com.intellij.codeInsight.completion._

class DeepAssocWrapperCbtr extends CompletionContributor {

	// does not contribute any completion actually, just
	// needed something to execute code on startup
	// probably not the best place to put it, but whatever
	try {
		// calling dynamically to avoid triggering static autoloader which
		// could disable this plugin if it has unresolved dependencies
		val wrapperPath = "org.klesun.deep_js_completion.resolvers.other_plugin_integration.DeepAssocWrapper"
		Class.forName(wrapperPath).newInstance().asInstanceOf[ {
			def registerDeepTypeProviders(): Unit
		}].registerDeepTypeProviders()
	} catch {
		case exc: Throwable => Console.println("Failed to pass js var phpdoc parser to deep-assoc plugin " + exc.getClass + " - " + exc.getMessage)
	}
}
