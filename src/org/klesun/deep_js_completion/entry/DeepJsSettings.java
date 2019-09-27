package org.klesun.deep_js_completion.entry;

import com.intellij.openapi.components.*;
import com.intellij.openapi.project.Project;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.Nullable;

@State(
    name = "DeepJsCompletionPluginSettings",
    storages = {
        @Storage(StoragePathMacros.WORKSPACE_FILE),
    }
)
public class DeepJsSettings implements PersistentStateComponent<DeepJsSettings> {
    public Integer explicitDepthLimit = 120;
    public Integer implicitDepthLimit = 35;
    public Integer totalExpressionLimit = 7500;
    public Boolean enableInTypescript = false;

    public static DeepJsSettings inst(Project project) {
        DeepJsSettings settings = ServiceManager.getService(project, DeepJsSettings.class);
        if (settings == null) {
            System.out.println("empty in service manager, creating new");
            settings = new DeepJsSettings();
        }
        return settings;
    }

    @Nullable
    @Override
    public DeepJsSettings getState() {
        return this;
    }

    public void loadState(DeepJsSettings deepSettings) {
        XmlSerializerUtil.copyBean(deepSettings, this);
    }
}
